module InferenceSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Map as M
import Control.Exception (evaluate)
import Data.Either

import Inference
import Model

getFinalSt component = snd $ runState (genConstraints component) emptyState

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
      length (constraints st) `shouldBe` 8 -- 2 * (2 * var/expr), 2 assn/expr, 2 flow/expr
    specify "modes" $ do
      let a = Assignment (Var "x2_dot") (Expr "-0.02*x2")
          f = Flow [a, a]
          st = getFinalSt (Mode "m" f)
      (freshCounter st) `shouldBe` 8 -- 2 vars, 2 exprs, 2 assignments, 1 flow,
                                     -- 1 mode
      length (constraints st) `shouldBe` 9 -- flows + 1
  describe "Constraint Simplification Unit Tests" $ do
    specify "One variable test" $ do
      let a = Var "a"
          user = [(a, High)]
          result = infer a user
      result `shouldSatisfy` isRight
      let Right env = result
      putStrLn $ show env
      join (env !? a) `shouldBe` Just High
