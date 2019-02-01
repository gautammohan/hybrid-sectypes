{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad.Writer
import Control.Monad
import Data.Map (Map, empty)
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Vector as V

main :: IO ()
main = someFunc

instance FromJSON Expr where
  parseJSON v = (pure Expr) <*> parseJSON v

instance FromJSON Flow where
  parseJSON v = (pure Flow) <*> parseJSON v

instance FromJSON Mode where
  parseJSON = withObject "mode object" $ \o ->
    do
      n <- o .: "name"
      f <- o .: "flow"
      pure $ Mode n (Flow [f])

instance FromJSON Transition where
  parseJSON = withObject "transition object" $ \o ->
    do
      g <- o .: "guard"
      src <- parseJSON =<< o .: "src"
      dest <- parseJSON =<< o .: "dest"
      pure $ Transition src dest (Guard g) (Reset [Expr ""])

instance FromJSON Model where
  parseJSON = withObject "model object" $ \o ->
    do
      ty <- o .: "ty"
      children <- o .: "children"
      if (ty :: String) == "parmodel"
        then
        pure Parallel <*> mapM parseJSON children
        else do
        ts <- mapM parseJSON =<< o .: "transitions"
        ms <- mapM parseJSON children
        pure $ Model ms ts

-- Basic Structure of a Hybrid system
-- NOTE: Currently ignoring expression parsing
type Var = String
type Name = String

data Expr = Expr String deriving (Eq, Show)

newtype Flow = Flow [Expr] deriving (Eq, Show)
newtype Guard = Guard Expr deriving (Show)
newtype Reset = Reset [Expr] deriving (Show)

-- Convention: Transition (Src) (Dest) Guard Reset
data Transition = Transition Mode Mode Guard Reset deriving (Show)
data Mode = Mode Name Flow deriving(Eq, Show)

data Model = Model [Mode] [Transition] | Parallel [Model] deriving (Show)

data Type = High | Low deriving (Show)
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
  check _ _ = return High
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
          then tell [Error "T-TRAN Violation" ("Implicit Flow: " ++
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

eh = [H,L]
el = [L]
fh = Flow eh
fl = Flow el
gh = Guard H
gl = Guard L
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

-- Testing the JSON parser code

s = fromString "{\"name\":\"Off1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"}"

s1 = fromString "{\"name\":\"Thermostat1\",\"children\":[{\"name\":\"Off1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"},{\"name\":\"On1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}],\"ty\":\"regmodel\",\"decomposition\":\"EXCLUSIVE_OR\",\"transitions\":[{\"guard\":\"{x1=10}\",\"src\":[],\"dest\":\"On1\"},{\"guard\":\"[x1 >= 30]\",\"src\":\"On1\",\"dest\":\"Off1\"},{\"guard\":\"[x1 <= 20]\",\"src\":\"Off1\",\"dest\":\"On1\"}]}"

s2 = fromString "{\"name\":\"Chart\",\"children\":[{\"name\":\"Thermostat1\",\"children\":[{\"name\":\"Off1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"},{\"name\":\"On1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}],\"ty\":\"regmodel\",\"decomposition\":\"EXCLUSIVE_OR\",\"transitions\":[{\"guard\":\"{x1=10}\",\"src\":{\"name\":\"InitialTransition\",\"flow\":\"\",\"ty\":\"mode\"},\"dest\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}},{\"guard\":\"[x1 >= 30]\",\"src\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"},\"dest\":{\"name\":\"Off1\",\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"}},{\"guard\":\"[x1 <= 20]\",\"src\":{\"name\":\"Off1\",\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"},\"dest\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}}]},{\"name\":\"Thermostat2\",\"children\":[{\"name\":\"Off2\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\"},{\"name\":\"On2\",\"children\":[],\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"}],\"ty\":\"regmodel\",\"decomposition\":\"EXCLUSIVE_OR\",\"transitions\":[{\"guard\":\"{x2=20}\n\",\"src\":{\"name\":\"InitialTransition\",\"flow\":\"\",\"ty\":\"mode\"},\"dest\":{\"name\":\"On2\",\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"}},{\"guard\":\"[x2 <= 30]\",\"src\":{\"name\":\"Off2\",\"ty\":\"mode\",\"flow\":\"Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\"},\"dest\":{\"name\":\"On2\",\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"}},{\"guard\":\"[x2 >= 50]\",\"src\":{\"name\":\"On2\",\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"},\"dest\":{\"name\":\"Off2\",\"ty\":\"mode\",\"flow\":\"Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\"}}]}],\"ty\":\"parmodel\",\"decomposition\":\"PARALLEL_AND\",\"transitions\":[]}"

s3 = fromString "{\"guard\":\"{x1=10}\",\"src\":{\"name\":\"InitialTransition\",\"flow\":\"\",\"ty\":\"mode\"},\"dest\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}}"
