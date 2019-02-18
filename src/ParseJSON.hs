{-# LANGUAGE OverloadedStrings #-}

module ParseJSON where

import Model
import ParseInternals

import Text.Parsec
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Text (unpack)

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
