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
  describe "Parsing CVar/CExpr/CFlow/Assignments works properly" $ do
    it "parses MATLAB variables correctly" $ do
      (parse' var "B2_dot") @=? Right (CVar "B2_dot")
      (parse' var "23bad_var") `shouldSatisfy` isLeft
      (parse' var "") `shouldSatisfy` isLeft
    it "parses Assignments properly into CVar/CExpr pairs" $ do
      (parse' assignment "x2_dot = -0.02*x2") @=?
        Right (CAssignment (CVar "x2_dot") (CExpr "-0.02*x2"))
      (parse' assignment "gr = gr - (d2 - (r2+p2))") @=?
        Right (CAssignment (CVar "gr") (CExpr "gr-(d2-(r2+p2))"))
    it "extracts all Vars from an CExpr" $ do
      (extractVars (CExpr "gr - (d2 - (r2+d2))")) @=?
        fmap CVar ["gr", "d2", "r2"]
    it "parses ;-separated Assignments with spaces" $ do
      (parse' assignments "p1=0; b1 = 0") @=?
        Right
          [CAssignment (CVar "p1") (CExpr "0"), CAssignment (CVar "b1") (CExpr "0")]
    it "parses a LabelString as the CFlow of a mode" $ do
      (parse' flow "Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;") @=?
        Right
          (CFlow
             [ CAssignment (CVar "x2_dot") (CExpr "-0.02*x2")
             , CAssignment (CVar "x2_out") (CExpr "x2")
             ])
  describe "Parsing Transitions works properly" $ do
    it "extracts a CGuard and CReset from a LabelString" $ do
      (parse' transition "[d1 - (r1+p1) < thresh || d2 - (r2+p2) >= 0] {m = 0;}") @=?
        Right
          ( CGuard (CExpr "d1-(r1+p1)<thresh||d2-(r2+p2)>=0")
          , CReset ([CAssignment (CVar "m") (CExpr "0")]))
    it "parses a LabelString with an empty CGuard" $ do
      (parse' transition "{p1 = 0; b1 = 0;}") @=?
        Right
          ( CGuard (CExpr "")
          , CReset
              [ CAssignment (CVar "p1") (CExpr "0")
              , CAssignment (CVar "b1") (CExpr "0")
              ])
    it "parses a LabelString with an empty CReset" $ do
      (parse' transition "[d1 - (r1+p1) < thresh || d2 - (r2+p2) >= 0]") @=?
        Right
          ( CGuard (CExpr "d1-(r1+p1)<thresh||d2-(r2+p2)>=0")
          , CReset [CAssignment (CVar "") (CExpr "")])
    it "parses a CReset with padded spaces" $ do
      (parse' transition "{ x = 0; y = 0; }" @=?
       Right
         ( CGuard (CExpr "")
         , CReset
             [CAssignment (CVar "x") (CExpr "0"), CAssignment (CVar "y") (CExpr "0")]))
    it "parses a CFlow with different separators / double newlines" $ do
      let ftext =
            "ModeB5\ndu:\nzb_dot = 1;\nVa_dot = fa; Va_out = Va; fa_dot = 0; fa_out = fa;\n\nVb_dot = fb; Vb_out = Vb; fb_dot = 0; fb_out = fb;\n\nVc_dot = fc; Vc_out = Vc; fc_dot = 0; fc_out = fc;\n\n\n"
      (parse' flow ftext) @=?
        Right
          (CFlow
             [ CAssignment (CVar "zb_dot") (CExpr "1")
             , CAssignment (CVar "Va_dot") (CExpr "fa")
             , CAssignment (CVar "Va_out") (CExpr "Va")
             , CAssignment (CVar "fa_dot") (CExpr "0")
             , CAssignment (CVar "fa_out") (CExpr "fa")
             , CAssignment (CVar "Vb_dot") (CExpr "fb")
             , CAssignment (CVar "Vb_out") (CExpr "Vb")
             , CAssignment (CVar "fb_dot") (CExpr "0")
             , CAssignment (CVar "fb_out") (CExpr "fb")
             , CAssignment (CVar "Vc_dot") (CExpr "fc")
             , CAssignment (CVar "Vc_out") (CExpr "Vc")
             , CAssignment (CVar "fc_dot") (CExpr "0")
             , CAssignment (CVar "fc_out") (CExpr "fc")
             ])
