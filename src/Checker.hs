{-# LANGUAGE FlexibleInstances #-}

module Checker where

import Model
import Control.Monad.Writer
import Control.Monad
import Data.Map (Map, empty)

type Var = String

data Type = High | Low deriving (Show, Read)
type Env = Map Var Type

-- Used to define Low <: High relation for security types in _expressions_
combineTypes :: Type -> Type -> Type
combineTypes Low Low = Low
combineTypes _ _ = High

-- Used to define High <: Low relation for security types in _commands_
validInContext :: Type -> Type -> Bool
Low `validInContext` High = False
_ `validInContext` _ = True

data Err = Err Name String deriving (Show)
type TC = Writer [Err]

class Checkable a where
  check :: Env -> a -> TC Type

instance Checkable [Expr] where
  check env exprs = foldr (liftM2 combineTypes) (return Low) (map (check env) exprs)

instance Checkable Expr where
  check _ (Expr "High") = return High
  check _ _ = return Low

instance Checkable Guard where
  check env (Guard expr) = check env expr

-- T-RESET
instance Checkable Reset where
  check env (Reset exprs) = check env exprs

-- T-FLOW
instance Checkable Flow where
  check env (Flow exprs) = check env exprs

instance Checkable Mode where
  check env (Mode _ flow) = check env flow

-- T-TRAN
instance Checkable Transition where
  check env (Transition src dest g reset) = do
    tySrc <- check env src
    tyDest <- check env dest
    tyGuard <- check env g
    tyReset <- check env reset

    sequence_ $ fmap (validComponentForType tyGuard) [tySrc, tyDest, tyReset]
    return tyGuard
      where
        validComponentForType :: Type -> Type -> TC ()
        validComponentForType ty tyComponent =
          if not $ tyComponent `validInContext` ty
          then tell [Err "T-TRAN Violation" ("Implicit Flow: " ++
                                      show tyComponent ++
                                      " type in " ++ show ty ++ " context")]
          else return ()

-- T-TRAN-APP rule for all valid pairs of transitions
instance Checkable Model where
  check _ (Parallel _) = undefined
  check env (Model modes transitions) = do
    mapM_ (check env) modes
    sequence_ $ fmap checkTransPair validTransPairings
    return Low --Default model "type" - really this is undefined for models
    where
      validTransPairings = [(t1,t2) | t1@(Transition src _ _ _) <- transitions
                                    , t2@(Transition _ dest _ _) <- transitions
                                    , src == dest]

      checkTransPair :: (Transition,Transition) -> TC ()
      checkTransPair (t1,t2) = do
        ty1 <- check env t1
        ty2 <- check env t2
        if ty1 `validInContext` ty2
          then return ()
          else tell [Err "[Name]" ("Transitions clash")]

-- A Simple Test Model

eh = [Expr "High",Expr "Low"]
el = [Expr "Low"]
fh = Flow eh
fl = Flow el
gh = Guard (Expr "High")
gl = Guard (Expr "Low")
rh = Reset eh
rl = Reset el
mh = Mode "High Mode" fh
ml = Mode "Low Mode" fl

-- Check T-TRAN rule violations
tr1 = Transition mh mh gh rl
tr2 = Transition mh ml gh rh
tr3 = Transition ml mh gh (Reset [])

-- Check T-TRAN-APP rule violations

trA = Transition mh mh gh rh
trB = Transition mh mh gl rl
