module Inference where

import Data.Map (Map, empty, (!?), insert)
import Control.Monad.State
import ParseInternals (extractVars)
import Text.Parsec (parse)
import Data.Maybe (mapMaybe, isJust)
import Data.List (delete, find)

import Model

data Type = High | Low deriving (Show, Read, Eq)

data Id
  = VId Var
  | MId Name
  | TId (Name,Name)
  deriving (Eq, Ord)

instance Show Id where
  show (VId v) = show v
  show (MId m) = m

type Env = Map Id TyVar
type TyEnv = Map TyVar Type
newtype TyVar = TyVar String deriving (Ord, Eq)

instance Show TyVar where
  show (TyVar s) = "'" ++ s

data CExpr = V TyVar | T Type deriving (Show, Eq)

-- Type Constraints that our typing rules can generate
data Constraint = CExpr :== CExpr -- LHS,RHS must have the same security type
                | CExpr :>= CExpr -- LHS has a higher sectype than RHS
                deriving(Show, Eq)

-- When given a substitution mapping a variable to a type, apply it to
-- constraints containing a matching variable and return the updated constraint.
-- If we make the substitution and the types mismatch, return an error. If the
-- variable does not match, return the constraint untouched.
subst :: TyVar -> Type -> Constraint -> Maybe Constraint
subst v ty c@(V v1 :== V v2)
  | v == v1 = Just (V v2 :== T ty)
  | v == v2 = Just (V v1 :== T ty)
subst v ty c@(V v' :== T ty')
  | v == v' && ty == ty' = Nothing
  | v == v' && ty /= ty' = error "Cannot Unify!" -- TODO more details later
  | otherwise = Just c
subst v ty (T ty' :== V v') = subst v ty (V v' :== T ty')
subst v ty c = Just c

-- If we can definitively infer the type of a variable from a constraint, return
-- the assignment
solve :: Constraint -> Maybe (TyVar, Type)
solve (V v :>= T High) = Just (v, High)
solve (T Low :>= V v) = Just (v, Low)
solve (V v :== T t) = Just (v, t)
solve (T t :== V v) = solve (V v :== T t)
solve _ = Nothing

-- simplify takes a list of constraints and recursively pops a solvable
-- constraint, solves it, and substitutes the result into the remaining
-- constraints repeatedly until no more solvable constraints remain. If a
-- constraint cannot be substituted, this errors out
simplify :: [Constraint] -> TyEnv
simplify cs = simplify' cs empty
  where
    simplify' [] env = env
    simplify' cs env =
      case (findSolvable cs) of
        Nothing -> env
        Just c -> simplify' updatedCs updatedEnv
          where
            -- Note this unwrap is always safe because findSolvable depends on
            -- solve and thus guarantees there was a solvable constraint in this
            -- case
            Just (v,ty) = solve c
            remaining = delete c cs
            updatedCs = mapMaybe (subst v ty) remaining
            updatedEnv = insert v ty env

    findSolvable :: [Constraint] -> Maybe Constraint
    findSolvable = find (isJust . solve)


data CGenState = CGenState { env :: Env
                           , constraints :: [Constraint]
                           , freshCounter :: Int
                           } deriving (Show)

setenv v tyv cgs = cgs {env = insert (getId v) tyv (env cgs)}

addConstraint :: Constraint -> State CGenState ()
addConstraint c = modify (\cgs -> cgs {constraints = c:(constraints cgs)})

addConstraints :: [Constraint] -> State CGenState ()
addConstraints = sequence_ . fmap addConstraint

emptyState = CGenState {env = empty, constraints = [], freshCounter = 0}

letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: State CGenState TyVar
fresh = do
  i <- gets freshCounter
  modify $ \cgs -> cgs {freshCounter = i+1}
  return $ TyVar (letters !! i)

-- Given a component of a model, create a fresh type variable for it and
-- generate the constraints between its tyvar and its children's tyvars
class GenConstraint a where
  genConstraints :: a -> State CGenState TyVar
  getId :: a -> Id

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
    addConstraints [V exprTyv :>= V varTyv | varTyv <- varTyvs]
    return exprTyv
  getId = undefined

instance GenConstraint Assignment where
  genConstraints (Assignment v e) = do
    assnTyv <- fresh
    varTyv <- getTyVar v
    exprTyv <- genConstraints e
    let cs = [V assnTyv :== V varTyv, V varTyv :>= V exprTyv]
    addConstraints cs
    return assnTyv
  getId = undefined

-- sectype of Flow must be the minimum of all the expressions it contains
instance GenConstraint Flow where
  genConstraints (Flow assignments) = do
    flowTyv <- fresh
    assnTyvs <- mapM genConstraints assignments
    addConstraints [V assnTyv :>= V flowTyv | assnTyv <- assnTyvs]
    return flowTyv

  getId = undefined

-- sectype of Mode is just the sectype of its flow
instance GenConstraint Mode where
  genConstraints m@(Mode _ flow) = do
    modeTyv <- fresh
    flowTyv <- genConstraints flow
    addConstraint (V modeTyv :== V flowTyv)
    modify $ setenv m modeTyv
    return modeTyv

  getId (Mode name _) = MId name

instance GenConstraint Guard where
  genConstraints (Guard e) = do
    guardTyv <- fresh
    exprTyv <- genConstraints e
    addConstraint (V guardTyv :== V exprTyv)
    return guardTyv

  getId = undefined

instance GenConstraint Reset where
  genConstraints (Reset exprs) = do
    resetTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    addConstraints [V exprTyv :>= V resetTyv | exprTyv <- exprTyvs]
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
          [ V resetTyv :>= V guardTyv
          , V srcTyv :>= V guardTyv
          , V dstTyv :>= V guardTyv
          , V transTyv :== V guardTyv
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
          addConstraint (V ty1 :>= V ty2)
    mapM_ constrainTransitions validTransPairings
    return tyModel

  getId = undefined
