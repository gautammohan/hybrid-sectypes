{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Model
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import Text.Parsec
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Text (unpack)

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
assignmentSep = try (string ";\n") <|> string ";" <?> "expected expr separator"

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

-- testing the parsec code
guar = "[High]\n{Low; Low}"
flo2 = fromString "{\"guard\":\"{High}\n[Low; Low]\",\"src\":{\"name\":\"M1\",\"ty\":\"mode\",\"flow\":\"M1\ndu:\nHigh;\nLow;\nHigh\"},\"dest\":{\"name\":\"M2\",\"ty\":\"mode\",\"flow\":\"M2\ndu:\nHigh;\nLow;\nHigh\"}}"

exps = "x2_dot = -0.02*x2;x2_out = x2;\n"
flo = "Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\""
p = "[d1 - (r1+p1) < thresh || d2 - (r2+p2) >= 0] {m = 0;}"
f = "Plan22\ndu:\np2_dot = m; B2_dot = b2; b2_dot = bm2;\nB2_out = B2;\np2_out = p2;\nn2_out = d2 - (r2+p2);"

f2 = "Initial Transition:"
s3 = fromString "{\"guard\":\"{x1=10}\",\"src\":{\"name\":\"InitialTransition\",\"flow\":\"Initial Transition: \",\"ty\":\"mode\"},\"dest\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}}"
-- Parsing from JSON

instance FromJSON Expr where
  parseJSON v = (pure Expr) <*> parseJSON v

instance FromJSON Flow where
  parseJSON = withText "flow expression" $ \t ->
    do
      case parse assignments "" (unpack t) of
        Right assns -> pure $ Flow assns
        Left err -> fail $ show err
-- instance FromJSON Flow where
--   parseJSON v = (pure Flow) <*> parseJSON v

instance FromJSON Mode where
  parseJSON = withObject "mode object" $ \o ->
    do
      n <- o .: "name"
      label <- o .: "flow"
      case parse flow "" label of
        Right f -> pure $ Mode n f
        Left err -> fail $ show err

instance FromJSON Transition where
  parseJSON = withObject "transition object" $ \o ->
    do
      label <- o .: "guard"
      src <- parseJSON =<< o .: "src"
      dest <- parseJSON =<< o .: "dest"
      case parse transition "" label of
        Right (guard,reset) -> pure $ Transition src dest guard reset
        Left err -> fail $ show err

instance FromJSON Model where
  parseJSON = withObject "model object" $ \o ->
    do
      ty <- o .: "ty"
      children <- o .: "children"
      if (ty :: String) == "parmodel"
        then
        pure Parallel <*> mapM parseJSON children
        else do
        ts <- mapM parseJSON =<< o .: "transitions"
        ms <- mapM parseJSON children
        pure $ Model ms ts

  -- Testing the JSON parser code

s = fromString "{\"name\":\"Off1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"}"

s1 = fromString "{\"name\":\"Thermostat1\",\"children\":[{\"name\":\"Off1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"},{\"name\":\"On1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}],\"ty\":\"regmodel\",\"decomposition\":\"EXCLUSIVE_OR\",\"transitions\":[{\"guard\":\"{x1=10}\",\"src\":[],\"dest\":\"On1\"},{\"guard\":\"[x1 >= 30]\",\"src\":\"On1\",\"dest\":\"Off1\"},{\"guard\":\"[x1 <= 20]\",\"src\":\"Off1\",\"dest\":\"On1\"}]}"

s2 = fromString "{\"name\":\"Chart\",\"children\":[{\"name\":\"Thermostat1\",\"children\":[{\"name\":\"Off1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"},{\"name\":\"On1\",\"children\":[],\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}],\"ty\":\"regmodel\",\"decomposition\":\"EXCLUSIVE_OR\",\"transitions\":[{\"guard\":\"{x1=10}\",\"src\":{\"name\":\"InitialTransition\",\"flow\":\"InitialTransition: \",\"ty\":\"mode\"},\"dest\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}},{\"guard\":\"[x1 >= 30]\",\"src\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"},\"dest\":{\"name\":\"Off1\",\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"}},{\"guard\":\"[x1 <= 20]\",\"src\":{\"name\":\"Off1\",\"ty\":\"mode\",\"flow\":\"Off1\ndu:\nx1_dot = -0.01*x1;\nx1_out = x1;\"},\"dest\":{\"name\":\"On1\",\"ty\":\"mode\",\"flow\":\"On1\ndu:\nx1_dot = -0.01*(x1-100);\nx1_out = x1;\"}}]},{\"name\":\"Thermostat2\",\"children\":[{\"name\":\"Off2\",\"children\":[],\"ty\":\"mode\",\"flow\":\"Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\"},{\"name\":\"On2\",\"children\":[],\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"}],\"ty\":\"regmodel\",\"decomposition\":\"EXCLUSIVE_OR\",\"transitions\":[{\"guard\":\"{x2=20}\n\",\"src\":{\"name\":\"InitialTransition\",\"flow\":\"InitialTransition: \",\"ty\":\"mode\"},\"dest\":{\"name\":\"On2\",\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"}},{\"guard\":\"[x2 <= 30]\",\"src\":{\"name\":\"Off2\",\"ty\":\"mode\",\"flow\":\"Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\"},\"dest\":{\"name\":\"On2\",\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"}},{\"guard\":\"[x2 >= 50]\",\"src\":{\"name\":\"On2\",\"ty\":\"mode\",\"flow\":\"On2\ndu:\nx2_dot = -0.02*(x2-100);\nx2_out = x2;\"},\"dest\":{\"name\":\"Off2\",\"ty\":\"mode\",\"flow\":\"Off2\ndu:\nx2_dot = -0.02*x2;\nx2_out = x2;\"}}]}],\"ty\":\"parmodel\",\"decomposition\":\"PARALLEL_AND\",\"transitions\":[]}"
