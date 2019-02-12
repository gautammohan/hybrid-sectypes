module Inference where

import Model
import Data.Map
import Control.Monad.State

newtype Var = Var String deriving (Ord, Eq)

instance Show Var where
  show (Var s) = s

data Type = High | Low deriving (Show, Read, Eq)

type Env = Map Var TyVar
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

letters = [1..] >>= flip replicateM ['a'..'z']
  
fresh :: State (Env, TyEnv, [Constraint], Int) TyVar
fresh = do
  i <- gets (\(_,_,_,i) -> i)
  modify (\(e,te,cs,i) -> (e,te,cs,i+1))
  return $ TyVar (letters !! i)


-- Given a component of a model, create a fresh type variable for it and
-- generate the constraints between its tyvar and its children's tyvars
class GenConstraint a where
  genConstraints :: a -> State (Env, TyEnv, [Constraint], Int) TyVar


instance GenConstraint Var where
  genConstraints v = do
    tyv <- fresh
    modify (\(e,te,cs,i) -> (insert v tyv e,te,cs,i))
    return tyv

emptyState = (empty,empty,[] :: [Constraint],0 :: Int)
