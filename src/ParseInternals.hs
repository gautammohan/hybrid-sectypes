module ParseInternals where

import Model

import Text.Parsec
import Data.List (nub)
import Data.List.Split (splitOn)

extractVars :: Parsec String () [Var]
extractVars = do
  allvars <- nonLetter *> var `sepEndBy` nonLetter
  return $ nub allvars

nonLetter = (many $ noneOf $ ['a'..'z'] ++ ['A'..'Z'])

-- Parse JSON text fields into Var, Expr, Assignment, Guard, Reset, Flow, Mode,
-- Transition

var :: Parsec String () Var
var = do
  char1 <- letter
  charRest <- many $ alphaNum <|> char '_'
  return (Var $ char1:charRest)

arith = "/*+-^()"
logic = "|&!"
cmp = "<>="

expr :: Parsec String () Expr
expr =
  many1
    (alphaNum <|>
     oneOf (arith ++ logic ++ cmp ++ "_ .") <?>
     "sequence of numbers, digits, arithmetic/comparison/logic operators, or spaces") >>=
  return . Expr

assignment :: Parsec String () Assignment
assignment = do
  v <- var
  e <- (many $ noneOf "=") *> char '=' *> expr
  return (Assignment v e)

assignmentSep :: Parsec String () String
assignmentSep =
  choice $ map try [string ";\n ", string ";\n", string "; ", string ";"]

assignments :: Parsec String () [Assignment]
assignments = assignment `sepEndBy` assignmentSep

flowHeader :: Parsec String () ()
flowHeader =
  skipMany (alphaNum <|> space <|> newline) >> char ':' >>
  skipMany newline <?> "expected flowheader"

flow :: Parsec String () Flow
flow = flowHeader >> assignments >>= return . Flow

reset :: Parsec String () Reset
reset = assignments >>= return . Reset

guard :: Parsec String () Guard
guard = expr >>= return . Guard

transition :: Parsec String () (Guard,Reset)
transition = do
  g <- option (Guard (Expr "")) $ between (char '[') (char ']') guard
  skipMany (space <|> newline)
  r <- option (Reset [Assignment (Var "") (Expr "")]) $ between (char '{') (char '}') reset
  return (g,r)
