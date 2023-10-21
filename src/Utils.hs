{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.Char (isDigit)
import           Data.Text (Text, breakOn, drop, intercalate, toLower)
import           Prelude   hiding (concat, drop)

extractContentInParentheses :: Text -> Text
extractContentInParentheses input = case breakOn "(" input of
  (_, rest) ->
    case breakOn ")" (drop 1 rest) of
      (inside, _) -> inside

extractFirstNumToDouble :: String -> Double
extractFirstNumToDouble input = case dropWhile (not . isDigit) input of
  "" -> 0.00
  xs -> case span (\c -> isDigit c || c == '.') xs of
    (intPart, rest) -> case reads intPart of
      [(number, "")] -> number
      _              -> extractFirstNumToDouble rest

concatAsPrintable :: [Text] -> Text
concatAsPrintable texts = "[" <> intercalate ", " (map (\text -> "\"" <> toLower text <> "\"") texts) <> "]"
