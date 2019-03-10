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
import Data.List
import Data.Graph.Inductive hiding ((&))

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
    case eitherDecode modelStr of
      Left err -> "Could not read model -- " ++ err
      Right model ->
        case inferVars (CModel model) anns of
          Left violation -> "Violation: " ++ formatViolation violation
          Right (varmap, remainders) ->
            case remainders of
              [] -> "Model satisfies noninterference"
              _ -> intercalate "\n" $ fmap formatWarning remainders
  where
    getVarTypes :: String -> [(Var, Type)]
    getVarTypes s =
      s & filter (/= ' ') & lines & map (splitOn ":") &
      map (\[a, b] -> (Var a, read b)) --HACK non-exhaustive pattern match!!!
    ----
    formatViolation (Violation cs) =
      "High information in " ++ highv ++
      " flows to low information in " ++
      lowv ++ "\n\nin component " ++ show greatestComponent
        --HACK pretty unsafe code here...
      where
        unwrap (CVar (Var v)) = v
        highv = unwrap . fst . head . tail $ cs
        lowv = unwrap . fst . last . init $ cs
        greatestComponent = snd $ maximumBy (compare `on` snd) cs
    formatWarning vars =
      "equivalent variables " ++
      intercalate "," (fmap (\(Var v) -> v) vars) ++ " are unspecified!"
