module InferenceSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Map as M

import Inference
import Model


spec :: Spec
spec = do
  describe "Constraint Generation Unit Tests" $ do
    specify "variables" $ do
      let oneV = genConstraints (Var "v")
      let (_, st) = runState oneV emptyState
      (freshCounter st) `shouldBe` 1
    specify "expressions" $ do
      let e = genConstraints (Expr "x2_dot+0.02*x2")
      let (_, st) = runState e emptyState
      (freshCounter st) `shouldBe` 3 --2 vars, 1 expr
      length (constraints st) `shouldBe` 2 --expr tyv relates to two var tyvs
    specify "assignments" $ do
      let a = genConstraints (Assignment (Var "x2_dot") (Expr "-0.02*x2"))
          (_, st) = runState a emptyState
      (freshCounter st) `shouldBe` 4 --2 vars, 1 expr, 1 assn
      length (constraints st) `shouldBe` 3 --assn relates to 1var, 1expr + 1var
    specify "flows" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          f = genConstraints (Flow [a, a])
          (_, st) = runState f emptyState
      (freshCounter st) `shouldBe` 7 --2 vars, 2 exprs, 2 assignments, 1 flow
      length (constraints st) `shouldBe` 8 -- 2 * (2 * var/expr), 2 assn/expr, 2 flow/expr
    specify "modes" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          f = Flow [a, a]
          m = genConstraints (Mode "m" f)
          (_, st) = runState m emptyState
      (freshCounter st) `shouldBe` 8 -- 2 vars, 2 exprs, 2 assignments, 1 flow, 1 mode
      length (constraints st) `shouldBe` 9 -- flows + 1
    specify "transitions" $ do pending
    specify "models" $ do pending
