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

-- Read the .json and .sectypes file to get a model with its user-supplied
-- security annotations. Infer the types of all variables in the system
-- and return the results:
--
-- On failure, return the conflicting inference steps.
--
-- Otherwise, print the inferred type of every variable in the model, and -
-- return unspecified variables (due to an underconstrained system) as -
-- warnings.
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
