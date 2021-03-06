{- |

Util is a few miscellaneous functions that are helpful to other Modules

-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Util (getAllVars, replaceVars, parseFromFile) where

import ParseInternals
import Model
import Data.List (intersect, nub)
import qualified Data.Map as M
import Data.Text (unpack, pack, replace)
import ParseJSON()
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B (readFile)

getAllVars' :: [Assignment] -> [Var]
getAllVars' as = nub $ concatMap getAllVars as

-- | Get all unique variables contained in a Component
getAllVars :: Component l -> [Var]
getAllVars (CVar "") = []
getAllVars v@(CVar _) = [v]
getAllVars e@(CExpr _) = nub $ extractVars e
getAllVars (CAssignment v e) = nub $ getAllVars v ++ getAllVars e
getAllVars (CFlow as) = getAllVars' as
getAllVars (CMode _ f) = getAllVars f
getAllVars (CGuard e) = getAllVars e
getAllVars (CReset as) = getAllVars' as
getAllVars (CTransition src dest g r) =
  nub $ getAllVars src ++ getAllVars dest ++ getAllVars g ++ getAllVars r
getAllVars (CModel ms ts) =
  nub $ concatMap getAllVars ms ++ concatMap getAllVars ts
getAllVars (CParallel ms) = nub $ concatMap getAllVars ms

-- | Given a dictionary of Var -> Var mappingss, replace each matching var with
-- the element in the map. This function has an unchecked invariant: a
-- replacement variable must not be present in the model. Otherwise, this code
-- could clobber those variables, depending on the order in which they are
-- processed.
replaceVars :: M.Map Var Var -> Component l -> Component l
replaceVars dict cv@(CVar _) = M.findWithDefault cv cv dict
replaceVars dict ce@(CExpr e) =
  let vars = extractVars ce
  -- find all the vars in the expression and look up their replacements
      replacements = zip vars (map (\v -> M.findWithDefault v v dict) vars)
  -- extract the literal var Strings to Data.Text
      replacements' =
        map (\(CVar v1, CVar v2) -> (pack v1, pack v2)) replacements
  -- iteratively replace each one
      e' = foldl (\str (old, new) -> replace old new str) (pack e) replacements'
  -- repack the replaced Text as a String in an Expression
  in CExpr (unpack e')
     -- if intersect (M.keys dict) (M.elems dict) == [] --HACK
     -- then CExpr (unpack e')
     -- else error "replacements intersect, possible incorrect substitution"
replaceVars dict (CAssignment v e) =
  CAssignment (replaceVars dict v) (replaceVars dict e)
replaceVars dict (CFlow as) = CFlow $ map (replaceVars dict) as
replaceVars dict (CGuard e) = CGuard $ replaceVars dict e
replaceVars dict (CReset as) = CReset $ map (replaceVars dict) as
replaceVars dict (CMode n f) = CMode n (replaceVars dict f)
replaceVars dict (CTransition src dest g r) = let
  src' = replaceVars dict src
  dest' = replaceVars dict dest
  g' = replaceVars dict g
  r' = replaceVars dict r
  in (CTransition src' dest' g' r')
replaceVars dict (CModel ms ts) = CModel (replace' ms) (replace' ts)
  where
    replace' :: [Component l] -> [Component l]
    replace' = map (replaceVars dict)
replaceVars dict (CParallel ms) = CParallel $ map (replaceVars dict ) ms

-- | Quick and unsafe way to get Models from JSON into Haskell to work with
parseFromFile :: String -> IO Model
parseFromFile fname = do
  raw <- B.readFile fname
  return $ case eitherDecode raw of
    Right m -> m
    Left err -> error err
