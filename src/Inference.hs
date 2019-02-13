module Inference where

import Model
import Data.Map
import Control.Monad.State
import Parse (extractVars)
import Text.Parsec (parse)

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
    modify $ setenv v tyv
    return tyv

instance GenConstraint Expr where
  genConstraints (Expr e) = do
    let vars = case parse extractVars "" e of
                 Left err -> error "Could not parse variables from expression"
                 Right vars -> vars
    exprTyv <- fresh
    -- sectype of expression must be greater than sectypes of the variables it
    -- contains
    varTyvs <- mapM genConstraints vars
    modify $ addconstr (TVar exprTyv :== (Max varTyvs))
    return exprTyv
