{-|

Abstract representation of EFSM-based hybrid systems.

-}
module Model where

-- | Name applies to Mode or Model names
type Name = String

-- | @Var@s correspond to variables in hybrid systems.
newtype Var = Var String deriving (Ord, Eq, Show)

-- | @Expr@s are represented as strings as currently we do not parse the
-- mathematical expressions inside them.
data Expr = Expr String deriving (Eq, Show, Ord)

-- | @Assignments@ are of the form: @Var@ = @Expr@.
data Assignment = Assignment Var Expr deriving (Eq, Show, Ord)

-- | A @Flow@ may have multiple assignments in it. It represents the dynamics
-- for a single mode.
newtype Flow = Flow [Assignment] deriving (Eq, Show, Ord)

-- | A @Guard@ contains a single expression which represents the eager switching
-- condition for a transition. This expression must evaluate to a boolean value.
newtype Guard = Guard Expr deriving (Eq, Show, Ord)

-- | The @Reset@ field of a transition represents assignments made to variables
-- when a transition is taken
newtype Reset = Reset [Assignment] deriving (Eq, Show, Ord)

-- | A @Mode@ may have a name, and must have a @Flow@ describig its dynamics.
data Mode = Mode Name Flow deriving(Eq, Show, Ord)

-- | A @Transition@ is defined in the standard way. The constructor convention
-- used throughout is: @Transition@ /src/ /dst/ /guard/ /reset/.
data Transition = Transition Mode Mode Guard Reset deriving (Eq, Show, Ord)

-- | A @Model@ is a recursive structure that represents a hierarchical hybrid
-- system. \"Leaf\" models contain @Mode@s and @Transition@s. It is implicit
-- that the set of modes in the mode field is equal to the modes specified as
-- the src and dest of every transition. This is not checked during
-- construction.

-- TODO could/should we check this?
data Model = Model [Mode] [Transition] | Parallel [Model] deriving (Eq, Show, Ord)
