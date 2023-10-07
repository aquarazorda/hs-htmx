{-# LANGUAGE OverloadedStrings #-}

module Data.WC.Category where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data WpCategory = WpCategory
  { name :: String,
    slug :: String
  } deriving (Show)

instance FromJSON WpCategory where
  parseJSON = withObject "WpCategory" $ \v ->
    WpCategory
      <$> v .: "name"
      <*> v .: "slug"