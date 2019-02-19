module ParseInternalsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Text.Parsec (parse, Parsec, ParseError)
import Data.Either (isLeft)

import Model
import ParseInternals

parse' :: Parsec String () a -> String -> Either ParseError a
parse' p str = parse p "" str

spec :: Spec
spec = do
  describe "Parsing Var/Expr/Flow/Assignments works properly" $ do
    it "parses MATLAB variables correctly" $ do
      (parse' var "B2_dot") @=? Right (Var "B2_dot")
      (parse' var "23bad_var") `shouldSatisfy` isLeft
      (parse' var "") `shouldSatisfy` isLeft
    it "parses Assignments properly into Var/Expr pairs" $ do
      (parse' assignment "x2_dot = -0.02*x2") @=?
        Right (Assignment (Var "x2_dot") (Expr "-0.02*x2"))
      (parse' assignment "gr = gr - (d2 - (r2+p2))") @=?
        Right (Assignment (Var "gr") (Expr "gr-(d2-(r2+p2))"))
    it "extracts all Vars from an Expr" $ do
      (parse' extractVars "gr - (d2 - (r2+d2))") @=?
        Right (fmap Var ["gr", "d2", "r2"])
    it "parses ;-separated Assignments with spaces" $ do
      (parse' assignments "p1=0; b1 = 0") @=?
        Right
          [Assignment (Var "p1") (Expr "0"), Assignment (Var "b1") (Expr "0")]
    it "parses a LabelString as the Flow of a mode" $ do
      (parse' flow "Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;") @=?
        Right
          (Flow
             [ Assignment (Var "x2_dot") (Expr "-0.02*x2")
             , Assignment (Var "x2_out") (Expr "x2")
             ])
  describe "Parsing Transitions works properly" $ do
    it "extracts a Guard and Reset from a LabelString" $ do
      (parse' transition "[d1 - (r1+p1) < thresh || d2 - (r2+p2) >= 0] {m = 0;}") @=?
        Right
          ( Guard (Expr "d1-(r1+p1)<thresh||d2-(r2+p2)>=0")
          , Reset ([Assignment (Var "m") (Expr "0")]))
    it "parses a LabelString with an empty Guard" $ do
      (parse' transition "{p1 = 0; b1 = 0;}") @=?
        Right
          ( Guard (Expr "")
          , Reset
              [ Assignment (Var "p1") (Expr "0")
              , Assignment (Var "b1") (Expr "0")
              ])
    it "parses a LabelString with an empty Reset" $ do
      (parse' transition "[d1 - (r1+p1) < thresh || d2 - (r2+p2) >= 0]") @=?
        Right
          ( Guard (Expr "d1-(r1+p1)<thresh||d2-(r2+p2)>=0")
          , Reset [Assignment (Var "") (Expr "")])
