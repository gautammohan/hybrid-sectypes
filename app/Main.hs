{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import ParseJSON()
import Model
import Inference
import Util

import System.Environment

import Control.Monad
import Control.Monad.Writer()
import Control.Monad.State()

import Data.Map.Lazy (toList)
import Data.Aeson (eitherDecode)
import qualified Data.Map as M()
import Data.List.Split
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Function
import Data.List
import Data.Bifunctor

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
  let anns = getVarTypes varStr
  putStrLn $
    either id id $ do
      model <- eitherDecode modelStr & first ("Could not read model --" ++)
      let unknownVars = fmap fst anns \\ getAllVars model
      unless (null unknownVars) $
        Left ("error, variables not present in model: " ++ show unknownVars)
      (varTys, remainders) <-
        inferVars (model :: Model) anns &
        first (\v -> "Violation: " ++ formatViolation v)
      pure $
        (intercalate "\n" $ fmtInferred <$> (toList varTys)) ++
        "\n" ++
        case remainders of
          [[]] ->
            "All variables have valid inferred type." ++
            " Model satisfies noninterference"
          _ ->
            "Unknown Variables: " ++
            intercalate " , " (formatWarning <$> (filter (/= []) remainders))
  where
    fmtInferred :: (Var, Type) -> String
    fmtInferred (CVar v, ty) = v ++ " : " ++ show ty
    formatViolation (Violation cs) =
      "High information in " ++
      highv ++
      " flows to low information in " ++
      lowv ++ "\n\nin component " ++ show greatestComponent
        --HACK pretty unsafe code here...
      where
        unwrap :: AnyC -> String
        unwrap (AnyC (CVar v)) = v
        highv = unwrap . fst . head . tail $ cs
        lowv = unwrap . fst . last . init $ cs
        greatestComponent = snd $ maximumBy (compare `on` snd) cs
    formatWarning vars =
      "[" ++ intercalate "," (fmap (\(CVar v) -> v) vars) ++ "]"

getVarTypes :: String -> [(Var, Type)]
getVarTypes s =
  s & filter (/= ' ') & lines & map (splitOn ":") &
  map (\[a, b] -> (CVar a, read b)) --HACK non-exhaustive pattern match!!!
