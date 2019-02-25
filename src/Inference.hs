{- |

The Inference module implements security type inference on hybrid systems
specified in the "Model" module. It implements constraint generation and
simplification, and attempts to solve the security types of as many variables as
possible given some initial specifications.

-}

{-# LANGUAGE DeriveFunctor #-}

module Inference where

import Prelude hiding (map)
import Data.Map (filterWithKey, mapKeys, Map, empty, (!?), insert, map)
import Control.Monad.State
import ParseInternals (extractVars)
import Text.Parsec (parse)
import Data.Maybe (mapMaybe, isJust)
import Data.List (delete, find)

import Model

-- | High security types are for secret data, Low can be observed publically
data Type
  = High
  | Low
  deriving (Show, Read, Eq)

-- | A wrapper around each modeltype used to map components to type variables
data Id
  = VId Var
  | MId Name
  | TId (Name,Name)             -- ^(src,dest)
  deriving (Eq, Ord)

instance Show Id where
  show (VId v) = show v
  show (MId m) = m
  show (TId (n1,n2)) = n1 ++ "/" ++ n2

-- | Maps system components to type variables
type Env = Map Id TyVar

-- | Maps type variables to types
type TyEnv = Map TyVar Type

-- | Variables for security types, should only take on H/L values
newtype TyVar = TyVar String deriving (Ord, Eq)

instance Show TyVar where
  show (TyVar s) = "'" ++ s

-- | LHS/RHS of constraints must be either a type variable or type
data CExpr = V TyVar | T Type deriving (Show, Eq)

-- | Relations between CExprs that our typing rules can generate
data Op = Equals | GreaterThan deriving (Eq)
instance Show Op where
  show Equals = "%=="
  show GreaterThan = "%>="

-- | Constructor for generic constraints
data GenericConstraint a = MkConstraint a Op a deriving(Eq, Functor)

instance (Show a) => Show (GenericConstraint a) where
  show (MkConstraint lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

-- | We only support constraints relating type variables and types
type Constraint = GenericConstraint CExpr

-- | Set two CExprs to be equal
(%==) :: Inference.CExpr -> Inference.CExpr -> Constraint
(%==) c1 c2 = MkConstraint c1 Equals c2

-- | Set c1 greater than c2
(%>=) :: Inference.CExpr -> Inference.CExpr -> Constraint
(%>=) c1 c2 = MkConstraint c1 GreaterThan c2

-- | When given a substitution mapping a variable to a type, apply it to
-- constraints containing a matching variable and return the updated constraint.
-- If we make the substitution and the types mismatch, return an error. If the
-- variable does not match, return the constraint untouched.
subst :: TyVar -> Type -> Constraint -> Maybe Constraint
subst v ty c = checkValid $ (fmap (replaceCExpr v ty) c)
  where
    replaceCExpr :: TyVar -> Type -> CExpr -> CExpr
    replaceCExpr v ty ce@(V var) =
      if v == var
        then (T ty)
        else ce
    replaceCExpr v ty ce = ce

    checkValid :: Constraint -> Maybe Constraint
    checkValid (MkConstraint (T Low) GreaterThan (T High)) =
      error "Cannot Unify"
    checkValid (MkConstraint (T t1) Equals (T t2)) =
      if t1 /= t2
        then error "Cannot Unify"
        else Nothing
    checkValid c = Just c


-- | Only if we can definitively infer the type of a variable from a constraint,
-- we return the assignment
solve :: Constraint -> Maybe (TyVar, Type)
solve (MkConstraint (V v) GreaterThan (T High)) = Just (v, High)
solve (MkConstraint (T Low) GreaterThan (V v)) = Just (v, Low)
solve (MkConstraint (V v) Equals (T t)) = Just (v,t)
solve (MkConstraint (T t) Equals (V v)) = solve (V v %== T t)
solve _ = Nothing

-- | simplify takes a list of constraints and recursively pops a solvable
-- constraint, solves it, and substitutes the result into the remaining
-- constraints repeatedly until no more solvable constraints remain. If a
-- constraint cannot be substituted, this errors out
simplify :: [Constraint] -> (TyEnv, [Constraint])
simplify cs = simplify' cs empty
  where
    simplify' [] env = (env, [])
    simplify' cs env =
      case (findSolvable cs) of
        Nothing -> (env, cs)
        Just c -> simplify' updatedCs updatedEnv
            -- Note this unwrap is always safe because findSolvable depends on
            -- solve and thus guarantees there was a solvable constraint in this
            -- case
          where Just (v, ty) = solve c
                remaining = delete c cs
                updatedCs = mapMaybe (subst v ty) remaining
                updatedEnv = insert v ty env
    findSolvable :: [Constraint] -> Maybe Constraint
    findSolvable = find (isJust . solve)

-- | Given a model and an initial partial specification of variable security
-- types, infer as many variable types as possible
infer :: Model -> [(Var,Type)] -> Map Var (Maybe Type)
infer model initVarTypes =
  let initState = execState (mapM addToEnv initVarTypes) emptyState
      CGenState {env = env, constraints = constraints} =
        execState (genConstraints model) initState
      (tyenv, _) = simplify constraints
   in getVarTypes env tyenv
  where
    -- Add a single initial constraint for each variable type specified by the
    -- user
    addToEnv :: (Var, Type) -> State CGenState ()
    addToEnv (v, ty) = do
      varTyv <- getTyVar v
      addConstraint $ V varTyv %== T ty

    -- For each variable, if its type variable is in the TyEnv, replace it with
    -- the associated type. Otherwise return Nothing.
    getVarTypes :: Env -> TyEnv -> Map Var (Maybe Type)
    getVarTypes e te = mapKeys (\(VId v) -> v) .
                       filterWithKey removeNonVars .
                       map (\k -> te !? k) $ e
      where
        removeNonVars k _ = case k of
                              VId _ -> True
                              otherwise -> False
-- | Holds a constraint list and environment that get built up as we generate
-- constraints for model components (that may recursively generate constraints
-- from subcomponents).
data CGenState = CGenState { env :: Env
                           , constraints :: [Constraint]
                           , freshCounter :: Int -- ^tracks index to generate
                                                 -- fresh variables
                           } deriving (Show)

-- | Helper function to update env with an (Id,Tyvar) entry
setenv v tyv cgs = cgs {env = insert (getId v) tyv (env cgs)}

-- | Helper function to add a new constraint to the list of constraints
addConstraint :: Constraint -> State CGenState ()
addConstraint c = modify (\cgs -> cgs {constraints = c:(constraints cgs)})

-- | Add multiple constraints at once
addConstraints :: [Constraint] -> State CGenState ()
addConstraints = sequence_ . fmap addConstraint

-- | new state to begin generating constraints
emptyState = CGenState {env = empty, constraints = [], freshCounter = 0}

-- | Return a fresh type variable relative to the current constraints being
-- generated
fresh :: State CGenState TyVar
fresh = do
  i <- gets freshCounter
  modify $ \cgs -> cgs {freshCounter = i+1}
  return $ TyVar (letters !! i)
  where
    -- KLUDGE probably n^2 if we have to recompute this list every time
    letters = [1..] >>= flip replicateM ['a'..'z'] -- infinite stream of unique
                                                   -- symbols
-- | The @GenConstraint@ class describes how to identify a model component, and
-- how to generate constraints between it and its subcomponents. The algorithm
-- to generate constraints comes from the typing rules that specify what
-- conditions must hold for no information to leak depending on the security
-- types of subcomponents.
class GenConstraint a where
  genConstraints :: a -> State CGenState TyVar
  getId :: a -> Id

-- | Make sure we check that a type variable does not already exist before
-- finding constraints, this prevents us from duplicating type variables for a
-- single component (ex. the same variable in multiple exprs/flows)
getTyVar :: (GenConstraint a) => a -> State CGenState TyVar
getTyVar component = do
  e <- gets env
  maybe (genConstraints component) return (e !? getId component)

instance GenConstraint Var where
  genConstraints v = do
    tyv <- fresh
    e <- gets env
    modify $ setenv v tyv
    return tyv

  getId = VId

-- sectype of expression must be greater than sectypes of the variables it
-- contains
instance GenConstraint Expr where
  genConstraints (Expr e) = do
    let vars =
          case parse extractVars "" e of
            Left err -> error "Could not parse variables from expression"
            Right vars -> vars
    -- if we have already seen the variable, return its type variable, otherwise
    -- generate its constraints
    exprTyv <- fresh
    varTyvs <- mapM getTyVar vars
    addConstraints [V exprTyv %>= V varTyv | varTyv <- varTyvs]
    return exprTyv
  getId = undefined

instance GenConstraint Assignment where
  genConstraints (Assignment v e) = do
    assnTyv <- fresh
    varTyv <- getTyVar v
    exprTyv <- genConstraints e
    let cs = [V assnTyv %== V varTyv, V varTyv %>= V exprTyv]
    addConstraints cs
    return assnTyv
  getId = undefined

-- sectype of Flow must be the minimum of all the expressions it contains
instance GenConstraint Flow where
  genConstraints (Flow assignments) = do
    flowTyv <- fresh
    assnTyvs <- mapM genConstraints assignments
    addConstraints [V assnTyv %>= V flowTyv | assnTyv <- assnTyvs]
    return flowTyv

  getId = undefined

-- sectype of Mode is just the sectype of its flow
--TODO mode is same type as its invariant, or if it has no invariants, the type
--of its flow
instance GenConstraint Mode where
  genConstraints m@(Mode _ flow) = do
    modeTyv <- fresh
    flowTyv <- genConstraints flow
    addConstraint (V modeTyv %== V flowTyv)
    modify $ setenv m modeTyv
    return modeTyv
  getId (Mode name _) = MId name

instance GenConstraint Guard where
  genConstraints (Guard e) = do
    guardTyv <- fresh
    exprTyv <- genConstraints e
    addConstraint (V guardTyv %== V exprTyv)
    return guardTyv

  getId = undefined

instance GenConstraint Reset where
  genConstraints (Reset exprs) = do
    resetTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    addConstraints [V exprTyv %>= V resetTyv | exprTyv <- exprTyvs]
    return resetTyv

  getId = undefined

  -- constraints generated from T-Tran rule
instance GenConstraint Transition where
  genConstraints t@(Transition src dst guard reset) = do
    -- if we have seen the mode, return its type variable, otherwise find its
    -- constraints
    transTyv <- fresh
    modify $ setenv t transTyv
    srcTyv <- getTyVar src
    dstTyv <- getTyVar dst
    guardTyv <- genConstraints guard
    resetTyv <- genConstraints reset
    let cs =
          [ V resetTyv %>= V guardTyv
          , V srcTyv %>= V guardTyv
          , V dstTyv %>= V guardTyv
          , V transTyv %== V guardTyv
          ]
    addConstraints cs
    return transTyv

  getId (Transition (Mode srcname _) (Mode dstname _) _ _) =
    TId (srcname, dstname)

-- constraints for T-Tran-App rule
instance GenConstraint Model where
  genConstraints m@(Model _ transitions) = do
    tyModel <- fresh
    let validTransPairings =
          [ (t1, t2)
          | t1@(Transition src _ _ _) <- transitions
          , t2@(Transition _ dest _ _) <- transitions
          , src == dest
          ]
    let
      constrainTransitions :: (Transition,Transition) -> State CGenState ()
      constrainTransitions (t1,t2) = do
          ty1 <- getTyVar t1
          ty2 <- getTyVar t2
          addConstraint (V ty1 %>= V ty2)
    mapM_ constrainTransitions validTransPairings
    return tyModel

  getId = undefined
