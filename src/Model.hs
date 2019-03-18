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

module Model where

import Data.Type.Equality
import Data.Map

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

type Var = Component 'Var
type Expr = Component 'Expr
type Assignment = Component 'Assignment
type Flow = Component 'Flow
type Guard = Component 'Guard
type Reset = Component 'Reset
type Mode = Component 'Mode
type Transition = Component 'Transition
type Model = Component 'Model
type Parallel = Component 'Model
