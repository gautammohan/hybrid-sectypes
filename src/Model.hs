{-# LANGUAGE StandaloneDeriving #-}
{- |
Abstract representation of EFSM-based hybrid systems.

-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Model
  ( Component(..)
  , Type(..)
  , Sing
  , AnyC(..)
  , Var
  , Expr
  , Assignment
  , Flow
  , Guard
  , Reset
  , Mode
  , Transition
  , Model
  , Parallel
  ) where

import Data.Type.Equality

type Name = String

data Label
  = Var
  | Expr
  | Assignment
  | Flow
  | Guard
  | Reset
  | Mode
  | Transition
  | Model
  deriving (Eq, Ord)

-- | A Component constructor creates components of a hybrid system.
data Component (a :: Label) where
  CVar :: String -> Component 'Var
  CExpr :: String -> Component 'Expr
  CAssignment :: Component 'Var -> Component 'Expr -> Component 'Assignment
  CFlow :: [Component 'Assignment] -> Component 'Flow
  CGuard :: Component 'Expr -> Component 'Guard
  CReset :: [Component 'Assignment] -> Component 'Reset
  CMode :: Name -> Component 'Flow -> Component 'Mode
  CTransition
    :: Component 'Mode
    -> Component 'Mode
    -> Component 'Guard
    -> Component 'Reset
    -> Component 'Transition
  CModel :: [Component 'Mode] -> [Component 'Transition] -> Component 'Model
  CParallel :: [Component 'Model] -> Component 'Model

deriving instance (Eq (Component l))
deriving instance (Ord (Component l))
deriving instance (Show (Component l))

data STy :: Label -> * where
  SVar :: STy 'Var
  SExpr :: STy 'Expr
  SAssn :: STy 'Assignment
  SFlow :: STy 'Flow
  SGuard :: STy 'Guard
  SReset :: STy 'Reset
  SMode :: STy 'Mode
  STrans :: STy 'Transition
  SModel :: STy 'Model

-- | Typeclass containing a singleton used to determine equality between AnyCs at the type level
class Sing l where
  sing :: STy l
instance Sing ('Var) where
  sing = SVar
instance Sing ('Expr) where
  sing = SExpr
instance Sing ('Assignment) where
  sing = SAssn
instance Sing ('Flow) where
  sing = SFlow
instance Sing 'Guard where
  sing = SGuard
instance Sing 'Reset where
  sing = SReset
instance Sing 'Mode where
  sing = SMode
instance Sing 'Transition where
  sing = STrans
instance Sing 'Model where
  sing = SModel

deriving instance (Show (STy l))
deriving instance (Eq (STy l))
deriving instance (Ord (STy l))

getSing :: (Sing l) => Component l -> STy l
getSing _ = sing

-- HACK This is not great, can I reflect a DataKind back to its data constructor
-- cleanly?
getLabel :: STy l -> Label
getLabel SVar = Var
getLabel SExpr = Expr
getLabel SAssn = Assignment
getLabel SFlow = Flow
getLabel SGuard = Guard
getLabel SReset = Reset
getLabel SMode = Mode
getLabel STrans = Transition
getLabel SModel = Model

cmpSTy :: STy a -> STy b -> Maybe (a :~: b)
cmpSTy SVar SVar = Just Refl
cmpSTy SExpr SExpr = Just Refl
cmpSTy SAssn SAssn = Just Refl
cmpSTy SFlow SFlow = Just Refl
cmpSTy SGuard SGuard = Just Refl
cmpSTy SReset SReset = Just Refl
cmpSTy SMode SMode = Just Refl
cmpSTy STrans STrans = Just Refl
cmpSTy SModel SModel = Just Refl
cmpSTy _ _ = Nothing

-- | Wrapper for a datatype that could be any Component
data AnyC where
  AnyC :: Sing l => Component l -> AnyC

deriving instance (Show AnyC)

instance (Eq AnyC) where
  (AnyC c1) == (AnyC c2) = case cmpSTy (getSing c1) (getSing c2) of
                             Just Refl -> c1 == c2
                             Nothing -> False

instance (Ord AnyC) where
  compare (AnyC c1) (AnyC c2) =
    let sty1 = getSing c1
        sty2 = getSing c2
     in case cmpSTy sty1 sty2 of
          Just Refl -> compare c1 c2
          Nothing -> compare (getLabel sty1) (getLabel sty2)

-- | Variables in a Hybrid System
type Var = Component 'Var
-- | Expressions (saved as raw MATLAB expression strings)
type Expr = Component 'Expr
-- | Assignments of the form Var = Expr
type Assignment = Component 'Assignment
-- | A Flow is a list of Assignments
type Flow = Component 'Flow
-- | A Guard expression represents the switching condition of a Transition
type Guard = Component 'Guard
-- | A Reset is a list of Assignments that are applied before transitioning
-- between modes
type Reset = Component 'Reset
-- | A Mode contains a Flow and Name
type Mode = Component 'Mode
-- | Transitions are constructed with the following convention: first arg is the
-- src mode and second arg is the dst mode
type Transition = Component 'Transition
-- | A model contains a list of Modes and Transitions (unchecked invariant: all
-- Modes in Transitions are present in the mode list)
type Model = Component 'Model
-- | A Parallel model holds Models or more Parallel models. Allows us to model
-- hierarchical hybrid systems as an n-ary tree
type Parallel = Component 'Model

-- | High security types are for secret data, Low can be observed publically
data Type
  = High
  | Low
  deriving (Show, Read, Eq, Ord)
