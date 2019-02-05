{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parse
import Model
import Checker
import Data.Aeson (decode)
import Control.Monad.Writer
import qualified Data.Map as M
import Data.List.Split
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Function
import System.Environment

main :: IO ()
main = do
  [modelFile,varFile] <- getArgs
  modelStr <- B.readFile modelFile
  varStr <- readFile varFile
  let Just model = decode modelStr
      env = createEnv varStr
  let (ty, errs) = runWriter $ check env (model :: Model)
  putStrLn $ show model
  putStrLn $ show errs
  where
    createEnv :: String -> Env
    createEnv s = s
                  & filter (== ' ')
                  & lines
                  & map (splitOn ":")
                  & map (\[a,b] -> (a, read b))
                  & M.fromDistinctDescList
