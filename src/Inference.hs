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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Inference where

import Prelude hiding (map)

import Model
import ParseInternals (extractVars)
import Util

import Data.Map
  ( Map
  , (!)
  , (!?)
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
import Control.Lens

-- | An NLabel represents an element of the DepGraph, which is either a
-- Component (wrapped in an AnyC) or a Type. The only two types that should be
-- present in a DepGraph are High and Low and represent "top" and "bottom".
data NLabel = LTy Type | LComp AnyC deriving (Show, Eq, Ord)

-- | A graph representing the relationships between the security types of
-- various components in a hybrid system. A directed edge (c1,c2) indicates the
-- security type of c1 must be less than or equal to c2, i.e. c1 cannot be High
-- and c2 cannot be Low.
type DepGraph = Gr NLabel NLabel

-- | Used to keep track of NLabels that have already been assigned an internal
-- node
type NodeMap = Map NLabel Node

-- | Relations between CExprs that our typing rules can generate
data Op = Equals | GreaterThan deriving (Eq, Ord)
instance Show Op where
  show Equals = "=="
  show GreaterThan = ">="

-- | Make Constraints take a parameter so we can derive Functor and save
-- ourselves some code
data GenericConstraint a = MkConstraint a Op a deriving (Eq, Ord, Functor)

instance (Show a) => Show (GenericConstraint a) where
  show (MkConstraint lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

-- | The only constraints we care about in this Inference are between NLabels.
-- This is to specify a relation between two components, or in the case of user
-- annotations, how a component relates to either High or Low in the DepGraph
type Constraint = GenericConstraint NLabel

-- | Constructor that specifies two components must have the _same_ security type
(%==) :: (Sing l, Sing l2) => Component l -> Component l2 -> Constraint
(%==) c1 c2 = MkConstraint (LComp (AnyC c1)) Equals (LComp (AnyC c2))

-- | Constructor that specifies c1 must be greater than c2
(%>=) :: (Sing l, Sing l2) => Component l -> Component l2 -> Constraint
(%>=) c1 c2 = MkConstraint (LComp (AnyC c1)) GreaterThan (LComp (AnyC c2))

-- | Used to track which Constraint produced a certain dependency in the DepGraph
type CMap = Map Constraint NLabel

-- | an SMap tracks which Variables have been seen and holds state to ensure we
-- are renaming them properly. This should have the same keys as a NodeMap in a
-- CGenState.
type SMap = Map Var Int

-- | Holds a constraint list and environment that get built up as we generate
-- constraints for model components (that may recursively generate constraints
-- from subcomponents).
data CGenState = CGenState
  { _nmap :: NodeMap -- ^Maps components we've seen to fresh DepGraph nodes
  , _constraints :: [Constraint] -- ^list of constraints that are built up
  , _cmap :: CMap -- ^tracks which constraints generated certain vars
  , _counter :: Node -- ^tracks fresh nodes
  , _seen :: SMap -- ^tracks state to ensure seen variables are renamed properly
  } deriving (Show)
makeLenses ''CGenState

-- | Placeholder var we use for history that doesn't matter that should be
-- scrubbed.
dummyVar :: Var
dummyVar = CVar "Dummy'"

-- | Helper function to add a new constraint to the list of constraints
addConstraint :: (Sing l) => Constraint -> Component l -> State CGenState ()
addConstraint constraint component = do
  constraints %= (constraint :)
  cmap %= insert constraint (LComp (AnyC component))

-- | Add multiple constraints at once
addConstraints :: (Sing l) => [Constraint] -> Component l -> State CGenState ()
addConstraints cs comp = sequence_ . fmap (flip addConstraint comp) $ cs

-- | New state to begin generating constraints
emptyState :: CGenState
emptyState =
  CGenState
    {_nmap = empty, _constraints = [], _cmap = empty, _seen = empty, _counter = 0}

-- | Given an (unseen) component or type, associate it with a fresh node and record that
-- association in the node map.
record :: NLabel -> State CGenState ()
record l = do
  n <- use counter
  counter += 1
  nmap %= insert l n

-- | Record a Component
recordNode :: (Sing l) => Component l -> State CGenState ()
recordNode c = record (LComp (AnyC c))

-- | Record a Node
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
  e <- use nmap
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
             addConstraint' $ f %>= m
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
             addConstraints' [g %>= t, r %>= g]
             if src == dst then
               return ()
               else
               addConstraints' $ [dst %>= src, src %>= g]
               where
                 -- lowEquivalent _ _ = undefined -- TODO
           CModel ms ts -> do
             mapM_ genConstraints ms
             mapM_ genConstraints ts
             addConstraints'
               [ t2 %>= t1
               | t1@(CTransition src1 dst1 _ _) <- ts
               , t2@(CTransition src2 dst2 _ _) <- ts
               , dst1 == src2 && src1 /= dst2
               ]
           CParallel ms -> mapM_ genConstraints' ms
             where genConstraints' :: Model -> State CGenState ()
                   genConstraints' (CParallel ms) = mapM_ genConstraints ms
                   genConstraints' m@(CModel _ _) = do
                     rmap <- use seen
                     let clashVars = clashes rmap m
                     renameVars <- mapM getFreshRename clashVars
                     let replacementMap = fromList $ zip clashVars renameVars
                         newModel = replaceVars replacementMap m
                     addConstraints'
                       [var %== rvar | (var, rvar) <- toList replacementMap]
                     genConstraints newModel
                     updateSeen $ getAllVars newModel
  where
    addConstraint' = flip addConstraint c
    addConstraints' = flip addConstraints c

-- | Given a Var, get a renamed version that is guaranteed to not be present in
-- any other model (basically postfixing it with an increasing number of ticks)
getFreshRename :: Var -> State CGenState Var
getFreshRename var@(CVar vtext) = do
  env <- use seen
  let i = env ! var
      ticks = take i $ repeat '\''
      env' = insert var (env ! var + 1) env
  modify (\cgs -> cgs {_seen = env'})
  return $ CVar (vtext ++ ticks)

-- | If we have seen new variables, add them to what we've seen in case they're
-- present in future Models down the road
updateSeen :: [Var] -> State CGenState ()
updateSeen vars = do
  seen %= addNewSeen vars
  where
    addNewSeen :: [Var] -> SMap -> SMap
    addNewSeen vars rmap =
      foldr (\v m -> insert v 1 m) rmap (vars \\ keys rmap)

-- | Find which Variables in a model clash with those we've already seen, i.e.
-- which ones need to be renamed
clashes :: SMap -> Model -> [Var]
clashes rmap m = intersect (keys rmap) (getAllVars m)

-- | For each component we have seen, we need to ensure they are leq High and
-- geq Low. These constraints are trivial since High and Low represent Top and
-- Bottom of our security annotations.
addHighLowConstraints :: State CGenState ()
addHighLowConstraints = do
  components <- liftM keys $ (use nmap)
  addConstraints
    ([MkConstraint (LTy High) GreaterThan c | c <- components])
    dummyVar
  addConstraints
    ([MkConstraint c GreaterThan (LTy Low) | c <- components])
    dummyVar

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
        nmap %= removeDummies
      st = execState inferSteps emptyState
   in buildDepGraph (st ^. nmap) (st ^. cmap) (st ^. constraints)

-- | Filter out dummy variables (used in a few corner cases)
removeDummies :: NodeMap -> NodeMap
removeDummies = filterWithKey $ \k _ -> k /= (LComp . AnyC $ dummyVar)

-- | A Violation is a path from High to Low in the DepGraph. It is represented
-- as a list of tuples (C1,C2) where C1 is the component node present in the
-- Path and C2 is the component that generated the constraint that created the
-- dependency in the DepGraph. This provides a trace of the offending
-- components, as well as which components' constraints produced the
-- dependencies that led to the violation.
data Violation = Violation [(AnyC, AnyC)]

deriving instance Show Violation

-- | Mapping Components to Types based on the inferred Dependency Graph
type CTyMap = Map AnyC Type

-- | A remainder is a list of Components in an SCC in the DepGraph that does not
-- contain High or Low, i.e. all the ariables must be the same type, but we
-- can't infer what that type is.
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
                  (AnyC dummyVar, AnyC dummyVar)
                lookup (n, e) = extract (fromJust $ lab g n, e)

-- | Extract the SCCs containing High and Low to find all components of that
-- type and convert it to a mapping from Component to type. Any other SCCs
-- represent components that must have the same type, but their type was not
-- determined from the partial specification. These untyped groupings are
-- "remainders" in a list.
getComponentTypes :: DepGraph -> (CTyMap, [Remainder])
getComponentTypes g = (tymap, remainder sccs)
    -- Find all components that are either high or low
  where
    tymap = fromList (mkPairs hscc High ++ mkPairs lscc Low)
    -- And group the remaining untyped components as remainders
    remainder :: [[Node]] -> [[AnyC]]
    remainder sccs =
      (fmap . fmap) (unsafelab g) $ delete hscc . delete lscc $ sccs
    -------
    mkPairs :: [Node] -> Type -> [(AnyC, Type)]
    mkPairs ns ty = fmap (\n -> (unsafelab g $ n, ty)) (filter isComponent ns)
    unsafelab g = extract . fromJust . lab g
    extract (LComp c) = c
    extract (LTy _) = error "should never extract an LTy"
    isComponent :: Node -> Bool
    isComponent n =
      case (fromJust . lab g) n of
        LTy _ -> False
        LComp _ -> True
    -------
    sccs = scc g
    getSCCWith n = head $ filter (n `elem`) sccs
    hscc = getSCCWith 0
    lscc = getSCCWith 1

-- | A Mapping from Vars to their Types; the final output we want our inference
-- to produce.
type VarTypes = Map Var Type

-- | Find the type of a subcomponent in a larger component, if it exists.
typeIn ::
     (Sing l, Sing l2)
  => Component l
  -> Component l2
  -> [(Var, Type)]
  -> Either Violation (Maybe Type)
typeIn innerC outerC anns =
  case checkDepGraph . (flip inferDepGraph anns) $ outerC of
    Left violation -> Left violation
    Right (ctymap, _) -> Right $ ctymap !? AnyC innerC

-- | A function that takes a component and a list of user-specified variable
-- type annotationsn and returns all variables that can be inferred and the
-- remainders. If there is a Violation, return that instead.
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
