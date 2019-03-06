module InferenceSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Map as M
import Control.Exception (evaluate)

import Inference
import Model


spec :: Spec
spec = do
  describe "Constraint Generation Unit Tests" $ do
    specify "variables" $ do
      let oneV = genConstraints (CVar $ Var "v")
      let (_, st) = runState oneV emptyState
      (counter st) `shouldBe` 1
    specify "expressions" $ do
      let e = genConstraints (CExpr $ Expr "x2_dot+0.02*x2")
      let (_, st) = runState e emptyState
      (counter st) `shouldBe` 3 --2 vars, 1 expr
      length (constraints st) `shouldBe` 2 --expr tyv relates to two var tyvs
    specify "assignments" $ do
      let a = genConstraints (CAssn $ Assignment (Var "x2_dot") (Expr "-0.02*x2"))
          (_, st) = runState a emptyState
      (counter st) `shouldBe` 4 --2 vars, 1 expr, 1 assn
      length (constraints st) `shouldBe` 3 --assn relates to 1var, 1expr + 1var
    specify "flows" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          f = genConstraints (CFlow $ Flow [a, a])
          (_, st) = runState f emptyState
      (counter st) `shouldBe` 5 -- flow relates to assn + 3 assn constraints
      length (constraints st) `shouldBe` 4
    specify "modes" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          f = Flow [a, a]
          m = genConstraints (CMode $ Mode "m" f)
          (_, st) = runState m emptyState
      (counter st) `shouldBe` 6 -- 2 vars, 2 exprs, 2 assignments, 1 flow, 1 mode
      length (constraints st) `shouldBe` 5 -- mode relates to flow + 4 assn
  describe ""
