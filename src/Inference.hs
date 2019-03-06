{- |

The Inference module implements security type inference on hybrid systems
specified in the "Model" module. It implements constraint generation and
simplification, and attempts to solve the security types of as many variables as
possible given some initial specifications.

-}

{-# LANGUAGE DeriveFunctor #-}

module Inference where

import Prelude hiding (map)

import Model
import ParseInternals (extractVars)

import Data.Map (fromList, keys, (!), toList, member, filterWithKey, mapKeys, Map, empty, (!?), insert, map)
import Data.Maybe (fromJust, mapMaybe, isJust)
import Data.List (nub, delete, find)
import Data.Graph.Inductive (scc, LPath(LP), lesp, esp, lab, LPath, DynGraph, Edge, Gr, LNode, Node, UEdge, mkGraph)
import qualified Data.Set as S
import Data.Tuple

import Text.Parsec (parse)

import Control.Monad.State

-- | High security types are for secret data, Low can be observed publically
data Type
  = High
  | Low
  deriving (Show, Read, Eq, Ord)

data Component
  = CVar Var
  | CExpr Expr
  | CAssn Assignment
  | CFlow Flow
  | CMode Mode
  | CGuard Guard
  | CReset Reset
  | CTrans Transition
  | CModel Model
  | CTy Type
  deriving (Eq, Show, Ord)

type DepGraph = Gr Component ()

type NodeMap = Map Component Node

data GenericConstraint a = MkConstraint a Op a deriving (Eq, Functor)

-- | Relations between CExprs that our typing rules can generate
data Op = Equals | GreaterThan deriving (Eq)
instance Show Op where
  show Equals = "=="
  show GreaterThan = ">="

instance (Show a) => Show (GenericConstraint a) where
  show (MkConstraint lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

type Constraint = GenericConstraint Component

(%==) :: Component -> Component -> Constraint
(%==) c1 c2 = MkConstraint c1 Equals c2

(%>=) :: Component -> Component -> Constraint
(%>=) c1 c2 = MkConstraint c1 GreaterThan c2

-- | Holds a constraint list and environment that get built up as we generate
-- constraints for model components (that may recursively generate constraints
-- from subcomponents).
data CGenState = CGenState { nmap :: NodeMap
                           , constraints :: [Constraint]
                           , counter :: Node -- ^tracks fresh nodes
                           } deriving (Show)

-- | Helper function to add a new constraint to the list of constraints
addConstraint :: Constraint -> State CGenState ()
addConstraint c = modify (\cgs -> cgs {constraints = c:(constraints cgs)})

-- | Add multiple constraints at once (pruning duplicates)
addConstraints :: [Constraint] -> State CGenState ()
addConstraints = sequence_ . fmap addConstraint . nub

-- | New state to begin generating constraints
emptyState = CGenState {nmap = empty, constraints = [], counter = 0}

-- | Given an (unseen) component, associate it with a fresh node and record that
-- association in the node map.
recordNode :: Component -> State CGenState ()
recordNode c = do
  e <- gets nmap
  n <- gets counter
  modify $ \cgs -> cgs {counter = n + 1, nmap = insert c n e}
  return ()

-- | Generate constraints for a "Component". We track the state as the
-- constraints are generated, and append constraints to a global list. When we
-- generate constraints for a component, we associate it with a Node. If we
-- encounter a syntactically identical component at a later point, we do not
-- generate constraints for it, since we know that they are duplicates. As we
-- generate constraints of a component. we also recursively generate constraints
-- of its subcomponents (i.e. generating constraints for the LHS var and RHS
-- expr in an Assignment). If we have seen any of the subcomponents before, the
-- recursion ends.
genConstraints :: Component -> State CGenState ()
genConstraints c = do
  e <- gets nmap
  if member c e
    then return ()
    else case c of
           c@(CVar _) -> recordNode c
           ce@(CExpr (Expr e)) ->
             let vars =
                   case parse extractVars "" e of
                     Left err ->
                       error "could not extract variables from expression"
                     Right vars' -> fmap (CVar) vars'
              in do recordNode ce
                    mapM_ genConstraints vars
                    addConstraints [ce %>= v | v <- vars]
           CAssn assn@(Assignment v e) -> do
             genConstraints (CExpr e)
             genConstraints (CVar v)
             recordNode (CAssn assn)
             addConstraints [CAssn assn %== CVar v, CVar v %>= CExpr e]
           CFlow f@(Flow as) -> do
             mapM_ (genConstraints . CAssn) as
             recordNode (CFlow f)
             addConstraints $ nub [CAssn a %>= CFlow f | a <- as]
           CMode m@(Mode _ f) -> do
             genConstraints (CFlow f)
             recordNode (CMode m)
             addConstraint $ CMode m %== CFlow f
           CGuard g@(Guard e) -> do
             genConstraints (CExpr e)
             recordNode (CGuard g)
             addConstraint $ CGuard g %== CExpr e
           CReset r@(Reset as) -> do
             mapM_ (genConstraints . CAssn) as
             recordNode (CReset r)
             addConstraints [CAssn a %>= CReset r | a <- as]
           CTrans t@(Transition src dst g r) -> do
             mapM_ genConstraints [CMode src, CMode dst, CGuard g, CReset r]
             recordNode (CTrans t)
             addConstraints
               [ CReset r %>= CGuard g
               , CMode src %>= CGuard g
               , CMode dst %>= CGuard g
               , CTrans t %== CGuard g
               ]
           (CModel (Model ms ts)) -> do
             mapM_ (genConstraints . CMode) ms
             mapM_ (genConstraints . CTrans) ts
             addConstraints
               [ CTrans t1 %>= CTrans t2
               | t1@(Transition src _ _ _) <- ts
               , t2@(Transition _ dest _ _) <- ts
               , src == dest
               ]
           (CTy _ ) -> return ()

-- | For each component we have seen, we need to ensure they are leq High and
-- geq Low. These constraints are trivial since High and Low represent Top and
-- Bottom of our security annotations.
addHighLowConstraints :: State CGenState ()
addHighLowConstraints = do
  components <- liftM keys $ gets nmap
  addConstraints $ [CTy High %>= c | c <- components]
  addConstraints $ [c %>= CTy Low | c <- components]

-- | Given a mapping of Components to nodes and a list of unique constraints
-- between components, create a dependency graph where nodes are labeled with
-- components, and a directed edge (a,b) represents that a must have a lower
-- security type than b.
buildDepGraph :: NodeMap -> [Constraint] -> DepGraph
buildDepGraph nmap cs = mkGraph nodes vertices
  where
    nodes = fmap swap . toList $ nmap
    vertices = concatMap (mkedge . fmap (nmap !)) cs
    mkedge (MkConstraint n1 Equals n2) = [(n1, n2, ()), (n2, n1, ())]
    mkedge (MkConstraint n1 GreaterThan n2) = [(n2,n1, ())]

-- | Convert initial variable annotations by user into constraints
addUserAnnotations :: [(Var,Type)] -> State CGenState ()
addUserAnnotations = addConstraints . fmap (\(v,ty) -> CVar v %== CTy ty)

-- | Given a "Component" and a list of user annotations, compute the constraints
-- for the components, Bottom/Top, and user types, and build a graph out of the
-- resulting constraints and subcomponents
inferDepGraph :: Component -> [(Var,Type)] -> DepGraph
inferDepGraph c vs =
  let inferSteps = do
        recordNode (CTy High)
        recordNode (CTy Low)
        genConstraints c
        addHighLowConstraints
        addUserAnnotations vs
      CGenState{nmap = nodemap, constraints = cs} = execState inferSteps emptyState
  in
    buildDepGraph nodemap cs

data Violation = Violation DepGraph (LPath ()) deriving(Show)

type CTyMap = Map Component Type
type Remainder = [Component]

-- | Check if a DepGraph is valid. If there exists a path from High to Low,
-- report the shortest such path as a violation. Otherwise, return a typing map
-- from Components to their types. Any components whose types are unspecified
-- are remainders, all components that must have the same (unspecified) type are
-- in the same remainder.
checkDepGraph :: DepGraph -> Either Violation (CTyMap, [Remainder])
checkDepGraph g = let
  high = 0
  low = 1
  in
    case (lesp high low g) of
      LP [] -> Right (getComponentTypes g)
      path -> Left (Violation g path)
  where

-- | Extract the SCCs containing High and Low to find all components of that
-- type and convert it to a mapping from Component to type. Any other SCCs
-- represent components that must have the same type, but their type was not
-- determined from the partial specification. These untyped groupings are
-- "remainders" in a list.
getComponentTypes :: DepGraph -> (CTyMap, [Remainder])
getComponentTypes g = (tymap, remainder sccs)
  where
    tymap = fromList (mkPairs hscc High ++ mkPairs lscc Low)
    remainder :: [[Node]] -> [[Component]] --and remove High and Low SCCs
    remainder sccs = (fmap . fmap) (unsafelab g) $ delete hscc . delete lscc $ sccs
    ------
    mkPairs :: [Node] -> Type -> [(Component, Type)]
    mkPairs ns ty = fmap (\n -> (unsafelab g $ n, ty)) ns
    unsafelab g = fromJust . lab g
    -------
    sccs = scc g
    getSCCWith n = head $ filter (n `elem`) sccs
    hscc = getSCCWith 0
    lscc = getSCCWith 1
