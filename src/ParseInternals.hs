{-|

ParseInternals contains functions used to parse various MATLAB strings in
Simulink/Stateflow models into the datatypes used by this package.

-}
module ParseInternals (extractVars
                      , assignments
                      , transition
                      , flow
                      , assignment
                      , var) where

import Model

import Text.Parsec
import Data.List (nub)
import Data.List.Split (splitOn)


-- | Extract all unique valid MATLAB variables from arithmetic expressions
extractVars :: Parsec String () [Var] --TODO change type to Parsec Expr () [Var]
extractVars = do
  allvars <- nonLetter *> var `sepEndBy` nonLetter
  return $ nub allvars
  where
    nonLetter = (many $ noneOf $ ['a'..'z'] ++ ['A'..'Z'])

-- | A MATLAB variable is a letter followed by any number of letters, numbers,
-- or underscores
var :: Parsec String () Var
var = do
  char1 <- letter
  charRest <- many $ alphaNum <|> char '_'
  return (Var $ char1:charRest)

expr :: Parsec String () Expr
expr =
  let arith = "/*+-^()"
      logic = "|&!"
      cmp = "<>="
   in many1
        (alphaNum <|>
         oneOf (arith ++ logic ++ cmp ++ "_ .") <?>
         "sequence of numbers, digits, arithmetic/comparison/logic operators, or spaces") >>=
      return . Expr . filter (/= ' ')

-- | Assignments have a @Var@ on the left and @Expr@ on the right separated by
-- an \"=\"
assignment :: Parsec String () Assignment
assignment = do
  v <- var
  e <- (many $ noneOf "=") *> char '=' *> expr
  return (Assignment v e)

assignmentSep :: Parsec String () String
assignmentSep =
  choice $ map try [string ";\n ", string ";\n", string "; ", string ";"]

-- | Assignments are valid MATLAB arithmetic syntax separated by
-- spaces\/newlines\/semicolons.
assignments :: Parsec String () [Assignment]
assignments = assignment `sepEndBy` assignmentSep

flowHeader :: Parsec String () ()
flowHeader =
  skipMany (alphaNum <|> space <|> newline <|> char '_') >> char ':' >>
  skipMany newline <?> "expected flowheader"

-- | Flow is of the form: \"FlowHeader: assignments...\". Make sure we get rid
-- of the header first
flow :: Parsec String () Flow
flow = flowHeader >> assignments >>= return . Flow

reset :: Parsec String () Reset
reset = do
  skipMany space
  a <- assignments
  skipMany space
  return $ Reset a

guard :: Parsec String () Guard
guard = expr >>= return . Guard

-- | A transition LabelString has the form [guard] {reset}, both of which may be
-- possibly empty. Guards are expressions and Resets maybe multiple separated
-- assignments
transition :: Parsec String () (Guard,Reset)
transition = do
  g <- option (Guard (Expr "")) $ between (char '[') (char ']') guard
  skipMany (space <|> newline)
  r <-
    option (Reset [Assignment (Var "") (Expr "")]) $
    between (char '{') (char '}') reset
  return (g, r)
