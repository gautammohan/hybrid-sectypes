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
    it "parses a Reset with padded spaces" $ do
      (parse' transition "{ x = 0; y = 0; }" @=?
       Right
         ( Guard (Expr "")
         , Reset
             [Assignment (Var "x") (Expr "0"), Assignment (Var "y") (Expr "0")]))
    it "parses a Flow with different separators / double newlines" $ do
      let ftext =
            "ModeB5\ndu:\nzb_dot = 1;\nVa_dot = fa; Va_out = Va; fa_dot = 0; fa_out = fa;\n\nVb_dot = fb; Vb_out = Vb; fb_dot = 0; fb_out = fb;\n\nVc_dot = fc; Vc_out = Vc; fc_dot = 0; fc_out = fc;\n\n\n"
      (parse' flow ftext) @=?
        Right
          (Flow
             [ Assignment (Var "zb_dot") (Expr "1")
             , Assignment (Var "Va_dot") (Expr "fa")
             , Assignment (Var "Va_out") (Expr "Va")
             , Assignment (Var "fa_dot") (Expr "0")
             , Assignment (Var "fa_out") (Expr "fa")
             , Assignment (Var "Vb_dot") (Expr "fb")
             , Assignment (Var "Vb_out") (Expr "Vb")
             , Assignment (Var "fb_dot") (Expr "0")
             , Assignment (Var "fb_out") (Expr "fb")
             , Assignment (Var "Vc_dot") (Expr "fc")
             , Assignment (Var "Vc_out") (Expr "Vc")
             , Assignment (Var "fc_dot") (Expr "0")
             , Assignment (Var "fc_out") (Expr "fc")
             ])
