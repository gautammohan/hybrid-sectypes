{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- |

The Inference module implements security type inference on hybrid systems
specified in the "Model" module. It implements constraint generation and
simplification, and attempts to solve the security types of as many variables as
possible given some initial specifications.

-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Inference where

import Prelude hiding (map)

import Model
import ParseInternals (extractVars)
import Util

import Data.Map
  ( Map
  , (!)
  , empty
  , filterWithKey
  , fromList
  , insert
  , keys
  , mapKeys
  , member
  , toList
  )
import Data.Maybe (fromJust, mapMaybe, isJust)
import Data.List (intersect, delete, find, nub, (\\))
import Data.Graph.Inductive
  (Gr
  , LPath(LP)
  , Node
  , lab
  , lesp
  , mkGraph
  , scc
  )
import qualified Data.Set as S
import Data.Tuple

import Control.Monad.State

-- | High security types are for secret data, Low can be observed publically
data Type
  = High
  | Low
  deriving (Show, Read, Eq, Ord)

data NLabel = LTy Type | LComp AnyC deriving (Show, Eq, Ord)

type DepGraph = Gr NLabel NLabel
type NodeMap = Map NLabel Node

-- | Relations between CExprs that our typing rules can generate
data Op = Equals | GreaterThan deriving (Eq, Ord)
instance Show Op where
  show Equals = "=="
  show GreaterThan = ">="

data GenericConstraint a = MkConstraint a Op a deriving (Eq, Ord, Functor)

instance (Show a) => Show (GenericConstraint a) where
  show (MkConstraint lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

type Constraint = GenericConstraint NLabel

(%==) :: (Sing l, Sing l2) => Component l -> Component l2 -> Constraint
(%==) c1 c2 = MkConstraint (LComp (AnyC c1)) Equals (LComp (AnyC c2))

(%>=) :: (Sing l, Sing l2) => Component l -> Component l2 -> Constraint
(%>=) c1 c2 = MkConstraint (LComp (AnyC c1)) GreaterThan (LComp (AnyC c2))

type CMap = Map Constraint NLabel
type RMap = Map Var Int
-- | Holds a constraint list and environment that get built up as we generate
  -- constraints for model components (that may recursively generate constraints
-- from subcomponents).
data CGenState = CGenState { nmap :: NodeMap
                           , constraints :: [Constraint]
                           , cmap :: CMap
                           , counter :: Node -- ^tracks fresh nodes
                           , rename :: RMap
                           }

deriving instance Show CGenState

-- | Helper function to add a new constraint to the list of constraints
addConstraint :: (Sing l) => Constraint -> Component l -> State CGenState ()
addConstraint constraint component = do
  modify
    (\cgs ->
       cgs
         { constraints = constraint : (constraints cgs)
         , cmap = insert constraint (LComp (AnyC component)) (cmap cgs)
         })

-- | Add multiple constraints at once
addConstraints :: (Sing l) => [Constraint] -> Component l -> State CGenState ()
addConstraints cs comp = sequence_ . fmap (flip addConstraint comp) $ cs

-- | New state to begin generating constraints
emptyState :: CGenState
emptyState =
  CGenState
    {nmap = empty, constraints = [], cmap = empty, rename = empty, counter = 0}

-- | Given an (unseen) component, associate it with a fresh node and record that
-- association in the node map.
record :: NLabel -> State CGenState ()
record l = do
  e <- gets nmap
  n <- gets counter
  modify $ \cgs -> cgs {counter = n + 1, nmap = insert l n e}
  return ()

recordNode :: (Sing l) => Component l -> State CGenState ()
recordNode c = record (LComp (AnyC c))

recordTy :: Type -> State CGenState ()
recordTy ty = record (LTy ty)

-- | Generate constraints for a "Component". We track the state as the
-- constraints are generated, and append constraints to a global list. When we
-- generate constraints for a component, we associate it with a Node. If we
-- encounter a syntactically identical component at a later point, we do not
-- generate constraints for it, since we know that they are duplicates. As we
-- generate constraints of a component. we also recursively generate constraints
-- of its subcomponents (i.e. generating constraints for the LHS var and RHS
-- expr in an Assignment). If we have seen any of the subcomponents before, the
-- recursion ends.
genConstraints :: (Sing l) => Component l -> State CGenState ()
genConstraints c = do
  e <- gets nmap
  if member (LComp (AnyC c)) e
    then return ()
    else case c of
           CVar _ -> recordNode c
           e@(CExpr _) ->
             let vars = extractVars e
              in do recordNode e
                    mapM_ genConstraints vars
                    addConstraints' [e %>= v | v <- vars]
           a@(CAssignment v e) -> do
             genConstraints e
             genConstraints v
             recordNode a
             addConstraints' [a %== v, v %>= e]
           f@(CFlow as) -> do
             mapM_ genConstraints as
             recordNode f
             addConstraints' $ nub [a %>= f | a <- as]
           m@(CMode _ f) -> do
             genConstraints f
             recordNode m
             addConstraint' $ m %== f
           g@(CGuard e) -> do
             genConstraints e
             recordNode g
             addConstraint' $ g %== e
           r@(CReset as) -> do
             mapM_ genConstraints as
             recordNode r
             addConstraints' [a %>= r | a <- as]
           t@(CTransition src dst g r) -> do
             genConstraints src
             genConstraints dst
             genConstraints g
             genConstraints r
             recordNode t
             addConstraints' [r %>= g, src %>= g, dst %>= g, t %== g]
           CModel ms ts -> do
             mapM_ genConstraints ms
             mapM_ genConstraints ts
             addConstraints'
               [ t1 %>= t2
               | t1@(CTransition src _ _ _) <- ts
               , t2@(CTransition _ dest _ _) <- ts
               , src == dest
               ]
           CParallel ms -> genConstraints' ms
             where genConstraints' :: [Component 'Model] -> State CGenState ()
                   genConstraints' [] = return ()
                   genConstraints' ((CParallel ms):rest) = do
                     mapM_ genConstraints ms
                     genConstraints' rest
                   genConstraints' (m@(CModel _ _):rest) = do
                     rmap <- gets rename
                     let clashVars = clashes rmap m
                     renameVars <- mapM getFreshRename clashVars
                     let replacementMap = fromList $ zip clashVars renameVars
                         newModel = replaceVars replacementMap m
                     addConstraints'
                       [var %== rvar | (var, rvar) <- toList replacementMap]
                     genConstraints newModel
                     updateRename $ getAllVars newModel
                     genConstraints' rest
  where
    addConstraint' = flip addConstraint c
    addConstraints' = flip addConstraints c

getFreshRename :: Var -> State CGenState Var
getFreshRename var@(CVar vtext) = do
  env <- gets rename
  let i = env ! var
      ticks = take i $ repeat '\''
      env' = insert var (env ! var + 1) env
  modify (\cgs -> cgs {rename = env'})
  return $ CVar (vtext ++ ticks)

updateRename :: [Var] -> State CGenState ()
updateRename vars = do
  rmap <- gets rename
  modify $ \cgs -> cgs {rename = addNewRenames rmap vars}
  where
    addNewRenames :: RMap -> [Var] -> RMap
    addNewRenames rmap vars = foldr (\v m -> insert v 1 m) rmap (vars \\ keys rmap)

clashes :: RMap -> Model -> [Var]
clashes rmap m = intersect (keys rmap) (getAllVars m)

-- | For each component we have seen, we need to ensure they are leq High and
-- geq Low. These constraints are trivial since High and Low represent Top and
-- Bottom of our security annotations.
addHighLowConstraints :: State CGenState ()
addHighLowConstraints = do
  components <- liftM keys $ gets nmap
  addConstraints
    ([MkConstraint (LTy High) GreaterThan c | c <- components])
    (CVar "Dummy'")
  addConstraints
    ([MkConstraint c GreaterThan (LTy Low) | c <- components])
    (CVar "Dummy'")

-- | Given a mapping of Components to nodes and a list of unique constraints
-- between components, create a dependency graph where nodes are labeled with
-- components, and a directed edge (a,b) represents that a must have a lower
-- security type than b. The directed edge is labeled with the component that
-- created the constraint, i.e. it's "parent".
buildDepGraph :: NodeMap -> CMap -> [Constraint] -> DepGraph
buildDepGraph nmap cmap cs = mkGraph nodes vertices
  where
    nodes = fmap swap . toList $ nmap
    vertices = concatMap mkedge cs
    mkedge c@(MkConstraint c1 Equals c2) =
      [(n1, n2, cmap ! c), (n2, n1, cmap ! c)]
      where
        n1 = nmap ! c1
        n2 = nmap ! c2
    mkedge c@(MkConstraint c1 GreaterThan c2) =
      [(nmap ! c2, nmap ! c1, cmap ! c)]

-- | Convert initial variable annotations by user into constraints
addUserAnnotations :: [(Var,Type)] -> State CGenState ()
addUserAnnotations vs =
  addConstraints
    (fmap (\(v, ty) -> MkConstraint (LComp (AnyC v)) Equals (LTy ty)) vs)
    (CVar "user'")

-- | Given a "Component" and a list of user annotations, compute the constraints
-- for the components, Bottom/Top, and user types, and build a graph out of the
-- resulting constraints and subcomponents
inferDepGraph :: (Sing l) => Component l -> [(Var,Type)] -> DepGraph
inferDepGraph c vs =
  let inferSteps = do
        recordTy High --top
        recordTy Low --bottom
        genConstraints c
        addHighLowConstraints
        addUserAnnotations vs
      CGenState {nmap = nodemap, cmap = cmap, constraints = cs} =
        execState inferSteps emptyState
   in buildDepGraph nodemap cmap cs

data Violation = Violation [(AnyC, AnyC)]

deriving instance Show Violation

type CTyMap = Map AnyC Type
type Remainder = [AnyC]

-- | Check if a DepGraph is valid. If there exists a path from High to Low,
-- report the shortest such path as a violation. Otherwise, return a typing map
-- from Components to their types. Any components whose types are unspecified
-- are remainders, all components that must have the same (unspecified) type are
-- in the same remainder.
checkDepGraph :: DepGraph -> Either Violation (CTyMap, [Remainder])
checkDepGraph g =
  let high = 0
      low = 1
   in case (lesp high low g) of
        LP [] -> Right (getComponentTypes g)
        LP path -> Left (Violation (fmap lookup path))
          where extract (LComp c, LComp d) = (c, d)
                extract (LComp c, LTy _) = (c, AnyC (CVar "user'"))
                extract (LTy _, LComp c) = (AnyC (CVar "user'"),c)
                extract (LTy _, LTy _) =
                  (AnyC (CVar "dummy what"), AnyC (CVar "dummy what"))
                lookup (n, e) = extract (fromJust $ lab g n, e)

-- | Extract the SCCs containing High and Low to find all components of that
-- type and convert it to a mapping from Component to type. Any other SCCs
-- represent components that must have the same type, but their type was not
-- determined from the partial specification. These untyped groupings are
-- "remainders" in a list.
getComponentTypes :: DepGraph -> (CTyMap, [Remainder])
getComponentTypes g = (tymap, remainder sccs)
  where
    -- Find all components that are either high or low
    tymap = fromList (mkPairs hscc High ++ mkPairs lscc Low)
    -- And group the remaining untyped components as remainders
    remainder :: [[Node]] -> [[AnyC]]
    remainder sccs =
      (fmap . fmap) (unsafelab g) $ delete hscc . delete lscc $ sccs
    -------
    mkPairs :: [Node] -> Type -> [(AnyC, Type)]
    mkPairs ns ty = fmap (\n -> (unsafelab g $ n, ty)) ns
    unsafelab g = extract . fromJust . lab g
    extract (LComp c) = c
    extract (LTy _) = (AnyC (CVar "Dummy"))
    -------
    sccs = scc g
    getSCCWith n = head $ filter (n `elem`) sccs
    hscc = getSCCWith 0
    lscc = getSCCWith 1


type VarTypes = Map Var Type

inferVars ::
     (Sing l)
  => Component l
  -> [(Var, Type)]
  -> Either Violation (VarTypes, [[Var]])
inferVars c anns =
  case checkDepGraph . (flip inferDepGraph anns) $ c of
    Left violation -> Left violation
    Right (ctymap, remainder) ->
      Right $ (vartypes, remainderVars) --inference only in terms of vars
      where
        -- extract only the variables from the Component TyMap
        vartypes = mapKeys unwrap $ filterWithKey (\c _ -> onlyVars c) ctymap
        -- extract only the variables from the Component remainders
        remainderVars = fmap (fmap unwrap . filter onlyVars) remainder
  where
    unwrap :: AnyC -> Var
    unwrap (AnyC v@(CVar _)) = v
    unwrap _ = error "This should never happen"
    onlyVars (AnyC (CVar _)) = True
    onlyVars _ = False
