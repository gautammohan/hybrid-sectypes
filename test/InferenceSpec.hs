module InferenceSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Map as M
import Control.Exception (evaluate)

import Inference
import Model

getFinalSt component = snd $ runState (genConstraints component) emptyState
spec :: Spec

spec :: Spec
spec = do
  describe "Constraint Generation Unit Tests" $ do
    specify "variables" $ do
      let st = getFinalSt (Var "v")
      (freshCounter st) `shouldBe` 1
    specify "expressions" $ do
      let st = getFinalSt (Expr "x2_dot+0.02*x2")
      (freshCounter st) `shouldBe` 3 --2 vars, 1 expr
      length (constraints st) `shouldBe` 2 --expr tyv relates to two var tyvs
    specify "assignments" $ do
      let st = getFinalSt (Assignment (Var "x2_dot") (Expr "-0.02*x2"))
      (freshCounter st) `shouldBe` 4 --2 vars, 1 expr, 1 assn
      length (constraints st) `shouldBe` 3 --assn relates to 1var, 1expr + 1var
    specify "flows" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          st = getFinalSt (Flow [a, a])
      (freshCounter st) `shouldBe` 7 --2 vars, 2 exprs, 2 assignments, 1 flow
      length (constraints st) `shouldBe` 8 -- 2 * (2 * var/expr), 2 assn/expr, 2
                                           -- flow/expr
    specify "modes" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          f = Flow [a, a]
          st = getFinalSt (Mode "m" f)
      (freshCounter st) `shouldBe` 8 -- 2 vars, 2 exprs, 2 assignments, 1 flow,
                                     -- 1 mode
      length (constraints st) `shouldBe` 9 -- flows + 1
  describe "Constraint Simplification Unit Tests" $ do
    let a = TyVar "a"; va = V a
        b = TyVar "b"; vb = V b
        c = TyVar "c"; vc = V c
        high = T High;
        low = T Low
    specify "simple subst tests" $ do
      subst a High (vb %>= va) `shouldBe` Just (vb %>= high)
    specify "unconstrained sets make no progress" $ do
      let init = [V a %== V b, V b %>= V c, V c %>= V a]
          (_, final) = simplify init
      final `shouldBe` init
    specify "simple: a high, b inferred high" $ do
      let init = [V b %>= V a, V a %== T High]
          (env, final) = simplify init
      env ! a `shouldBe` High
      env ! b `shouldBe` High
    specify "simple: a high, b inferred high, c leftover" $ do
      let init = [V a %>= T High, V b %>= V a, V b %>= V c]
          (env, final) = simplify init
      env ! a `shouldBe` High
      env ! b `shouldBe` High
      final `shouldContain` [T High %>= V c]
    specify "simple error: a is high and low" $ do
      let init = [V a %>= T High, V a %== T Low]
      evaluate (simplify init) `shouldThrow` anyErrorCall
