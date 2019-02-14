module Inference where

import Model
import Data.Map
import Control.Monad.State
import Parse (extractVars)
import Text.Parsec (parse)

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

setenv v tyv cgs = cgs {env = insert (getId v) tyv (env cgs)}
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
    modify $ addconstr (TVar exprTyv :== Max varTyvs)
    return exprTyv
  getId = undefined

-- sectype of Flow must be the minimum of all the expressions it contains
instance GenConstraint Flow where
  genConstraints (Flow exprs) = do
    flowTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    modify $ addconstr (TVar flowTyv :== Min exprTyvs)
    return flowTyv

  getId = undefined

-- sectype of Mode is just the sectype of its flow
instance GenConstraint Mode where
  genConstraints m@(Mode _ flow) = do
    modeTyv <- fresh
    flowTyv <- genConstraints flow
    modify $ addconstr (TVar modeTyv :== TVar flowTyv)
    modify $ setenv m modeTyv
    return modeTyv

  getId (Mode name _) = MId name

instance GenConstraint Guard where
  genConstraints (Guard e) = do
    guardTyv <- fresh
    exprTyv <- genConstraints e
    modify $ addconstr (TVar guardTyv :== TVar exprTyv)
    return guardTyv

  getId = undefined

instance GenConstraint Reset where
  genConstraints (Reset exprs) = do
    resetTyv <- fresh
    exprTyvs <- mapM genConstraints exprs
    modify $ addconstr (TVar resetTyv :== Min exprTyvs)
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
          [ TVar resetTyv :>= TVar guardTyv
          , TVar srcTyv :>= TVar guardTyv
          , TVar dstTyv :>= TVar guardTyv
          , TVar transTyv :== TVar guardTyv
          ]
    sequence_ . fmap (modify . addconstr) $ cs
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
          modify $ addconstr (TVar ty1 :>= TVar ty2)
    mapM_ constrainTransitions validTransPairings
    return tyModel

  getId = undefined
