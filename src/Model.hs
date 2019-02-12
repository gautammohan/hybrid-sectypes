module Model where

-- Basic Structure of a Hybrid system
-- NOTE: Currently ignoring expression parsing

type Name = String
newtype Var = Var String deriving (Ord, Eq)
instance Show Var where
  show (Var s) = s

data Expr = Expr String deriving (Eq, Show)
newtype Flow = Flow [Expr] deriving (Eq, Show)
newtype Guard = Guard Expr deriving (Show)
newtype Reset = Reset [Expr] deriving (Show)

-- Convention: Transition (Src) (Dest) Guard Reset
data Transition = Transition Mode Mode Guard Reset deriving (Show)
data Mode = Mode Name Flow deriving(Eq, Show)

data Model = Model [Mode] [Transition] | Parallel [Model] deriving (Show)
