{-# LANGUAGE OverloadedStrings #-}
module Main where

import ParseJSON
import Model
import Inference

import Control.Monad.Writer
import Control.Monad.State

import Data.Aeson (eitherDecode)
import qualified Data.Map as M
import Data.List.Split
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Function
import System.Environment

-- | Takes two arguments: a JSON file output from exportSLSFModel.m and a
-- corresponding file containing security-type annotations of variables in the
-- model. Main attempts to read and parse the model and security type
-- specification and infer the types of as many variables as it can in the
-- model. If the security types do not agree, the program crashes. Otherwise,
-- the inferred variable types are returned, as well as any types that were
-- unable to be inferred, in the case of an underconstrained system.
main :: IO ()
main = do
  [modelFile, varFile] <- getArgs --HACK deprecated
  modelStr <- B.readFile modelFile
  varStr <- readFile varFile
  let vs = getVarTypes varStr
  putStrLn $
    case eitherDecode modelStr of
      Left err -> "Could not read model -- " ++ err
      Right model -> show $ infer model vs
  where
    getVarTypes :: String -> [(Var, Type)]
    getVarTypes s =
      s & filter (/= ' ') & lines & map (splitOn ":") &
      map (\[a, b] -> (Var a, read b)) --HACK non-exhaustive pattern match!!!
