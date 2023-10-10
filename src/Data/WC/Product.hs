{-# LANGUAGE OverloadedStrings #-}

module Data.WC.Product where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.WC.Category (WpCategory (..))

newtype MetaData = MetaData {value :: String}
  deriving (Show)

instance FromJSON MetaData where
  parseJSON = withObject "MetaData" $ \v ->
    MetaData
      <$> v .: "value"

data WpPost = WpPost
  { id :: Int,
    name :: String,
    price :: String,
    slug :: String,
    short_description :: String,
    stock_quantity :: Int,
    permalink :: String,
    related_ids :: Maybe [Int],
    categories :: [WpCategory],
    meta_data :: [MetaData]
  }

instance FromJSON WpPost where
  parseJSON = withObject "WpPost" $ \v ->
    WpPost
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "price"
      <*> v .: "slug"
      <*> v .: "short_description"
      <*> v .: "stock_quantity"
      <*> v .: "permalink"
      <*> v .: "related_ids"
      <*> v .: "categories"
      <*> v .: "meta_data"