{-# LANGUAGE OverloadedStrings #-}

module Data.WC.Category where

import           Data.Aeson (FromJSON, parseJSON, withObject, (.!=), (.:),
                             (.:?))

data WpCategory = WpCategory
  { wpCatId :: Int,
    name    :: String,
    slug    :: String,
    count   :: Int
  } deriving (Show)

instance FromJSON WpCategory where
  parseJSON = withObject "WpCategory" $ \v ->
    WpCategory
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "slug"
      <*> v .:? "count" .!= 0
