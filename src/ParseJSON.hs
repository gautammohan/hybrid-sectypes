{-# LANGUAGE OverloadedStrings #-}

module ParseJSON where

import Model
import ParseInternals

import Text.Parsec
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Text (unpack)

instance FromJSON Expr where
  parseJSON v = (pure Expr) <*> parseJSON v

instance FromJSON Flow where
  parseJSON = withText "flow expression" $ \t ->
    do
      case parse assignments "" (unpack t) of
        Right assns -> pure $ Flow assns
        Left err -> fail $ show err

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
