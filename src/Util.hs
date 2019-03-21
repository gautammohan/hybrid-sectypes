{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import ParseInternals
import Model
import Data.List (intersect, nub)
import qualified Data.Map as M
import Data.Text (unpack, pack, replace)

getAllVars' :: [Assignment] -> [Var]
getAllVars' as = nub $ concatMap getAllVars as

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
  in if intersect (M.keys dict) (M.elems dict) == [] --HACK
     then CExpr (unpack e')
     else error "replacements intersect, possible incorrect substitution"
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
