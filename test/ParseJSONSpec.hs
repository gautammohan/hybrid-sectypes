{-# LANGUAGE OverloadedStrings #-}

module ParseJSONSpec (spec) where

import SimpleModel
import Model
import ParseJSON

import Data.Either (isRight)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B (readFile)
import Test.Hspec
import System.Cmd
import GHC.IO.Exception

mkExportCmd modelname =
  matlabExecute $
  "addpath('models'); exportSLSFModel(" ++
  squote modelname ++ ", " ++ (squote $ takeWhile (/= '.') modelname) ++ "); quit"
  where
    squote s = "'" ++ s ++ "'"
    quote s = "\"" ++ s ++ "\""
    matlabExecute cmd =
      "/Applications/MATLAB_R2018b.app/bin/matlab -nodesktop -nosplash -nojvm -r " ++
      quote cmd

-- Given a modelname "model.mdl", produce its JSON from MATLAB and return the
-- JSON file.
-- HACK: this uses deprecated "System.Cmd" and fails unsafely
genModelJSON :: String -> IO String
genModelJSON modelName =  do
  err <- system . mkExportCmd $ modelName
  case err of
   ExitSuccess -> return (takeWhile (/= '.') modelName ++ ".json")
   ExitFailure i -> error ("failed with: " ++ show i)

spec :: Spec
spec = do
  describe "long integration tests" $ do
    it "parses simpleModel.slx" $ do
      let jsonFile = "simple_model.json"
      rawJSON <- B.readFile "simple_model.json"
      case eitherDecode rawJSON of
        Right model -> (model :: Model) `shouldBe` simpleModel
        Left err -> fail err
