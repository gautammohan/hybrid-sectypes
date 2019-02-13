module Inference where

import Model
import Data.Map
import Control.Monad.State
import Parse (extractVars)
import Text.Parsec (parse)

data Type = High | Low deriving (Show, Read, Eq)

data Id = VId Var | MId Name deriving (Eq, Ord)
instance Show Id where
  show (VId v) = show v
  show (MId m) = m

type Env = Map Id TyVar
type TyEnv = Map TyVar Type
newtype TyVar = TyVar String deriving (Ord, Eq)

instance Show TyVar where
  show (TyVar s) = "'" ++ s

-- Constraint Expression, what can be on the LHS or RHS of a constraint
data CExpr = TVar TyVar -- type variable corresponding to expression/component
           | Max [TyVar] -- max over some TyVars
           | Min [TyVar] -- minimum over some TyVars
           deriving(Show) 

-- Type Constraints that our typing rules can generate
data Constraint = CExpr :== CExpr -- LHS,RHS must have the same security type
                | CExpr :>= CExpr -- LHS has a higher sectype than RHS
                | CExpr :<= CExpr -- LHS has a lower sectype than RHS
                deriving(Show)

data CGenState = CGenState { env :: Env
                           , constraints :: [Constraint]
                           , freshCounter :: Int
                           } deriving (Show)

setenv v tyv cgs = cgs {env = insert v tyv (env cgs)}
addconstr c cgs = cgs {constraints = c:(constraints cgs)}

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

instance GenConstraint Var where
  genConstraints v = do
    tyv <- fresh
    e <- gets env
    modify $ setenv (VId v) tyv
    return tyv

-- sectype of expression must be greater than sectypes of the variables it
-- contains
instance GenConstraint Expr where
  genConstraints (Expr e) = do
    let vars = case parse extractVars "" e of
                 Left err -> error "Could not parse variables from expression"
                 Right vars -> vars
    -- if we have already seen the variable, return its type variable, otherwise
    -- generate its constraints
    e <- gets env
    let getVar v = maybe (genConstraints v) return (e !? VId v)

    exprTyv <- fresh
    varTyvs <- mapM getVar vars 
    modify $ addconstr (TVar exprTyv :== Max varTyvs)
    return exprTyv

-- sectype of Flow must be the minimum of all the expressions it contains
instance GenConstraint Flow where
  genConstraints (Flow exprs) = do
    flowTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    modify $ addconstr (TVar flowTyv :== Min exprTyvs)
    return flowTyv

-- sectype of Mode is just the sectype of its flow
instance GenConstraint Mode where
  genConstraints (Mode name flow) = do
    modeTyv <- fresh
    flowTyv <- genConstraints flow
    modify $ addconstr (TVar modeTyv :== TVar flowTyv)
    modify $ setenv (MId name) modeTyv
    return modeTyv

instance GenConstraint Guard where
  genConstraints (Guard e) = do
    guardTyv <- fresh
    exprTyv <- genConstraints e
    modify $ addconstr (TVar guardTyv :== TVar exprTyv)
    return guardTyv

instance GenConstraint Reset where
  genConstraints (Reset exprs) = do
    resetTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    modify $ addconstr (TVar resetTyv :== Min exprTyvs)
    return resetTyv

  -- constraints generated from T-Tran rule
instance GenConstraint Transition where
  genConstraints (Transition src dst guard reset) = do
    -- if we have seen the mode, return its type variable, otherwise find its
    -- constraints
    e <- gets env
    let getMode m@(Mode name _) =
          maybe (genConstraints m) return (e !? MId name)

    transTyv <- fresh
    srcTyv <- getMode src
    dstTyv <- getMode dst
    guardTyv <- genConstraints guard
    resetTyv <- genConstraints reset
    let cs =
          [ TVar resetTyv :>= TVar guardTyv
          , TVar srcTyv :>= TVar guardTyv
          , TVar dstTyv :>= TVar guardTyv
          , TVar transTyv :== TVar guardTyv
          ]
    sequence_ . fmap (modify . addconstr) $ cs
    return transTyv
