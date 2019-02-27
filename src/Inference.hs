{- |

The Inference module implements security type inference on hybrid systems
specified in the "Model" module. It implements constraint generation and
simplification, and attempts to solve the security types of as many variables as
possible given some initial specifications.

-}

{-# LANGUAGE DeriveFunctor #-}

module Inference where

import Prelude hiding (map)
import Data.Map
  ( Map
  , (!)
  , (!?)
  , empty
  , filterWithKey
  , fromList
  , insert
  , map
  , mapKeys
  , mapWithKey
  )
import Control.Monad.State
import ParseInternals (extractVars)
import Text.Parsec (parse)
import Data.Maybe (catMaybes, isJust)
import Data.List (delete, find)
import Data.Either
import Data.Function ((&))

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

-- | Maps type variables to their types, as well as keeping track of the type
-- variable used when deducing the tyvar's type, i.e. it's \"history\" variable.
type TyEnv = Map TyVar (Type, TyVar)

-- | Variables for security types, should only take on H/L values
newtype TyVar = TyVar String deriving (Ord, Eq)

instance Show TyVar where
  show (TyVar s) =
    "'" ++ s

-- | Relations between type variables that our typing rules can generate
data Op = Equals | GreaterThan deriving (Eq)
instance Show Op where
  show Equals = "%=="
  show GreaterThan = "%>="

-- | Constructor for generic constraints
data GenericConstraint a = MkC a Op a deriving(Eq,Functor)

instance (Show a) => Show (GenericConstraint a) where
  show (MkC lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

-- | Specialized constructor for constraints between type variables
type Constraint = GenericConstraint TyVar

-- | Set two type variables to be equal
(%==) :: TyVar -> TyVar -> Constraint
(%==) v1 v2 = MkC v1 Equals v2

-- | Set v1 greater than v2
(%>=) :: TyVar -> TyVar -> Constraint
(%>=) v1 v2 = MkC v1 GreaterThan v2

data Violation = Violation TyEnv [(TyVar,TyVar)] deriving (Show)

-- | Only if we can definitively infer the type of a variable from a constraint,
-- we return the assignment
solve :: TyEnv -> Constraint -> Maybe (TyVar, (Type, TyVar))
solve env c@(MkC l _ r) =
  case fmap (env !?) c of
    MkC Nothing Equals (Just _) -> solve env (MkC r Equals l)
    MkC (Just (t, _)) Equals Nothing -> Just (r, (t, l))
    MkC (Just (Low, _)) GreaterThan Nothing -> Just (r, (Low, l))
    MkC Nothing GreaterThan (Just (High, _)) -> Just (l, (High, r))
    otherwise -> Nothing

-- | Given an environment, check if a constraint is still valid. Eliminate it if
-- it is. If the constraint is invalid, return the two offending variables. If a
-- constraint cannot be eliminated under the current environment, leave it
-- untouched
checkValid :: TyEnv -> Constraint -> Either (TyVar, TyVar) (Maybe Constraint)
checkValid env c@(MkC lv _ rv) =
  if lv == rv
    then Right Nothing --LHS = RHS
    else case (fmap (env !?) c) of
           MkC (Just (tl, _)) Equals (Just (tr, _)) ->
             if tl /= tr
               then Left (lv, rv) --ty
               else Right Nothing
           MkC (Just (tl, _)) GreaterThan (Just (tr, _)) ->
             if tl == Low && tr == High
               then Left (lv, rv) --low cannot be greater than high
               else Right Nothing
           otherwise -> Right (Just c)

-- | simplify takes a list of constraints and recursively pops a solvable
-- constraint, solves it, and substitutes the result into the remaining
-- constraints repeatedly until no more solvable constraints remain. If a
-- constraint cannot be substituted, this errors out
simplify :: [Constraint] -> TyEnv -> Either Violation TyEnv
simplify [] env = Right env
simplify cs env =
  let findSolvable = find $ isJust . (solve env)
   in case (findSolvable cs) of
        Nothing -> Right env
        Just c ->
          case violations of
            [] -> simplify (catMaybes newCs) newEnv
            otherwise -> Left (Violation env violations)
          where Just res = solve env c --safe unwrap because findSolvable
                                           --depends on solve
                newEnv = (uncurry insert) res env
                remainingCs = delete c cs
                (violations, newCs) =
                  partitionEithers . fmap (checkValid newEnv) $ remainingCs

genInitialEnvs :: [(Var,Type)] -> (TyEnv, CGenState)
genInitialEnvs pairs =
  let initState = execState (mapM addVarToEnv pairs) emptyState
      e = env initState
      tyenv =
        fromList $ fmap (\(v, ty) -> ((e ! VId v), (ty, TyVar "user'"))) pairs
   in (tyenv, initState)
  where
    addVarToEnv :: (Var, Type) -> State CGenState ()
    addVarToEnv (v, ty) = do
      varTyv <- getTyVar v
      modify $ setenv v varTyv

-- | Given a model and an initial partial specification of variable security
-- types, infer as many variable types as possible
infer ::
     (GenConstraint a)
  => a
  -> [(Var, Type)]
  -> Either Violation (Map Var (Maybe Type))
infer component initVarTypes =
  let (initTyEnv, CGenState {constraints = constraints, env = env}) =
        genInitialEnvs initVarTypes
   in case simplify constraints initTyEnv of
        Right tyenv -> Right $ getVarTypes env tyenv
        Left err -> Left err
    -- For each variable, if its type variable is in the TyEnv, replace it with
    -- the associated type. Otherwise return Nothing.
  where
    getVarTypes :: Env -> TyEnv -> Map Var (Maybe Type)
    getVarTypes e te =
      e & map (\k -> te !? k) & fmap (liftM fst) & filterWithKey removeNonVars &
      mapKeys (\(VId v) -> v)
      where
        removeNonVars k v =
          case k of
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
    addConstraints [exprTyv %>= varTyv | varTyv <- varTyvs]
    return exprTyv
  getId = undefined

instance GenConstraint Assignment where
  genConstraints (Assignment v e) = do
    assnTyv <- fresh
    varTyv <- getTyVar v
    exprTyv <- genConstraints e
    let cs = [assnTyv %== varTyv, varTyv %>= exprTyv]
    addConstraints cs
    return assnTyv
  getId = undefined

-- sectype of Flow must be the minimum of all the expressions it contains
instance GenConstraint Flow where
  genConstraints (Flow assignments) = do
    flowTyv <- fresh
    assnTyvs <- mapM genConstraints assignments
    addConstraints [assnTyv %>= flowTyv | assnTyv <- assnTyvs]
    return flowTyv

  getId = undefined

-- sectype of Mode is just the sectype of its flow
--TODO mode is same type as its invariant, or if it has no invariants, the type
--of its flow
instance GenConstraint Mode where
  genConstraints m@(Mode _ flow) = do
    modeTyv <- fresh
    flowTyv <- genConstraints flow
    addConstraint (modeTyv %== flowTyv)
    modify $ setenv m modeTyv
    return modeTyv
  getId (Mode name _) = MId name

instance GenConstraint Guard where
  genConstraints (Guard e) = do
    guardTyv <- fresh
    exprTyv <- genConstraints e
    addConstraint (guardTyv %== exprTyv)
    return guardTyv

  getId = undefined

instance GenConstraint Reset where
  genConstraints (Reset exprs) = do
    resetTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    addConstraints [exprTyv %>= resetTyv | exprTyv <- exprTyvs]
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
          [ resetTyv %>= guardTyv
          , srcTyv %>= guardTyv
          , dstTyv %>= guardTyv
          , transTyv %== guardTyv
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
          addConstraint (ty1 %>= ty2)
    mapM_ constrainTransitions validTransPairings
    return tyModel

  getId = undefined
