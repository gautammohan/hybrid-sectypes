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

import Data.Map (member, filterWithKey, mapKeys, Map, empty, (!?), insert, map)
import Data.Maybe (mapMaybe, isJust)
import Data.List (delete, find)
import Data.Graph.Inductive (Gr, LNode, Node, Edge, DynGraph)
import qualified Data.Set as S

import Text.Parsec (parse)

import Control.Monad.State

-- | High security types are for secret data, Low can be observed publically
data Type
  = High
  | Low
  deriving (Show, Read, Eq)

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

seen :: Component -> State CGenState Bool
seen c = (member c) <$> (gets nmap)

-- | Helper function to add a new constraint to the list of constraints
addConstraint :: Constraint -> State CGenState ()
addConstraint c = modify (\cgs -> cgs {constraints = c:(constraints cgs)})

-- | Add multiple constraints at once
addConstraints :: [Constraint] -> State CGenState ()
addConstraints = sequence_ . fmap addConstraint

-- | new state to begin generating constraints
emptyState = CGenState {nmap = empty, constraints = [], counter = 0}

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
             addConstraints [CAssn a %>= CFlow f | a <- as]
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
