import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Text.Parsec (parse)
import Data.Either (isLeft)

import Model
import Parse

main :: IO ()
main =
  hspec $ do
    describe "Parsing Var/Expr/Flow/Assignments works properly" $ do
      it "parses MATLAB variables correctly" $ do
        (parse var "" "B2_dot") @=? Right (Var "B2_dot")
        (parse var "" "n2") @=? Right (Var "n2")
        (parse var "" "23bad_var") `shouldSatisfy` isLeft
        (parse var "" "") `shouldSatisfy` isLeft
