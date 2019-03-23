module InferenceSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Map as M
import Control.Exception (evaluate)
import Data.Either

import Inference
import Model
import Util

spec :: Spec
spec = do
  describe "Constraint Generation Unit Tests" $ do
    specify "variables" $ do
      let oneV = genConstraints (CVar "v")
      let (_, st) = runState oneV emptyState
      (counter st) `shouldBe` 1
    specify "expressions" $ do
      let e = genConstraints (CExpr "x2_dot+0.02*x2")
      let (_, st) = runState e emptyState
      (counter st) `shouldBe` 3 --2 vars, 1 expr
      length (constraints st) `shouldBe` 2 --expr tyv relates to two var tyvs
    specify "assignments" $ do
      let a =
            genConstraints (CAssignment (CVar "x2_dot") (CExpr "-0.02*x2"))
          (_, st) = runState a emptyState
      (counter st) `shouldBe` 4 --2 vars, 1 expr, 1 assn
      length (constraints st) `shouldBe` 3 --assn relates to 1var, 1expr + 1var
    specify "flows" $ do
      let a = CAssignment (CVar "x2_dot") (CExpr "-0.02*x2")
          f = genConstraints (CFlow [a, a])
          (_, st) = runState f emptyState
      (counter st) `shouldBe` 5 -- flow relates to assn + 3 assn constraints
      length (constraints st) `shouldBe` 4
    specify "modes" $ do
      let a = CAssignment (CVar "x2_dot") (CExpr "-0.02*x2")
          f = CFlow [a, a]
          m = genConstraints (CMode "m" f)
          (_, st) = runState m emptyState
      (counter st) `shouldBe` 6 -- 2 vars, 2 exprs, 2 assignments, 1 flow, 1 mode
      length (constraints st) `shouldBe` 5 -- mode relates to flow + 4 assn
  describe "Dependency Graph Unit Tests" $ do
      --- Preliminary Declarations for typing ease
      let a = CVar "a"
          b = CVar "b"
          c = CVar "c"
          aplusb = CExpr "a + b"
          aplusc = CExpr "a + c"
      ----- Actual Tests Start Here
      specify "simple inference: a, a:High" $ do
        let g = inferDepGraph (a) [(a, High)]
            result = checkDepGraph g
        result `shouldSatisfy` isRight
        let Right (m, _) = result
        m !? (AnyC a) `shouldBe` Just High
      specify "simple inference: a = a + b, a:Low" $ do
        let g = inferDepGraph (CAssignment a aplusb) [(a, Low)]
            result = checkDepGraph g
        result `shouldSatisfy` isRight
        let Right (m, _) = result
        m !? (AnyC b) `shouldBe` Just Low
      specify "no inference possible: a = a + b, b:Low" $ do
        let g = inferDepGraph (CAssignment a aplusb) [(b, Low)]
            result = checkDepGraph g
        result `shouldSatisfy` isRight
        let Right (m, _) = result
        m !? (AnyC a) `shouldBe` Nothing
      specify "Invalid typing: a = a + b, a:Low b:High" $ do
        let g = inferDepGraph (CAssignment a aplusb) [(a, Low), (b, High)]
            result = checkDepGraph g
        result `shouldSatisfy` isLeft
      specify "Harder Inference: a = a + b, c = a + c, c:low" $ do
        let g =
              inferDepGraph (CFlow [CAssignment a aplusb, CAssignment c aplusc])
                [(c, Low)]
            result = checkDepGraph g
        result `shouldSatisfy` isRight
        let Right (m, _) = result
        m !? (AnyC a) `shouldBe` Just Low
        m !? (AnyC b) `shouldBe` Just Low
      let mfile = "fully_dependent_mode.json"
      specify (mfile) $ do
        m <- parseFromFile mfile
        let result = inferVars m [(CVar "x1_dot",Low)]
        result `shouldSatisfy` isRight
        let Right (vmap,_) = result
        elems vmap `shouldBe` [Low, Low,Low,Low,Low,Low]
        getAllVars m `shouldMatchList` keys vmap
