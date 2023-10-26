{-# LANGUAGE OverloadedStrings #-}

module Data.WC.Product where

import Data.Aeson
  ( FromJSON
  , ToJSON (toJSON)
  , object
  , parseJSON
  , withObject
  , (.:)
  , (.=)
  )
import Data.Text (Text)
import Data.WC.Category

newtype MetaData = MetaData {value :: String}
  deriving (Show)

instance FromJSON MetaData where
  parseJSON = withObject "MetaData" $ \v ->
    MetaData
      <$> v
      .: "value"

data WpPost = WpPost
  { wpPostId :: Int
  , wpPostName :: String
  , wpPostPrice :: String
  , wpPostSlug :: String
  , wpPostShortDescription :: String
  , wpPostStockQuantity :: Int
  , wpPostPermalink :: String
  , wpPostRelatedIds :: Maybe [Int]
  , wpPostCategories :: [WpCategory]
  , wpPostMetaData :: [MetaData]
  }

instance FromJSON WpPost where
  parseJSON = withObject "WpPost" $ \v ->
    WpPost
      <$> v
      .: "id"
      <*> v
      .: "name"
      <*> v
      .: "price"
      <*> v
      .: "slug"
      <*> v
      .: "short_description"
      <*> v
      .: "stock_quantity"
      <*> v
      .: "permalink"
      <*> v
      .: "related_ids"
      <*> v
      .: "categories"
      <*> v
      .: "meta_data"

data WpProduct = WpProduct
  { wpProductName :: Text
  , wpProductType :: Text
  , wpProductPrice :: Text
  , wpProductDescription :: Text
  , wpProductCategories :: [Int]
  , wpProductStatus :: Text
  , wpProductManageStock :: Bool
  , wpProductStockQuantity :: Int
  , wpProductImages :: Text
  }
  deriving (Show)

instance ToJSON WpProduct where
  toJSON (WpProduct name type' price description categories status manageStock stockQuantity image) =
    object
      [ "name" .= name
      , "type" .= type'
      , "regular_price" .= price
      , "short_description" .= description
      , "categories" .= fmap (\c -> object ["id" .= c]) categories
      , "status" .= status
      , "manage_stock" .= manageStock
      , "stock_quantity" .= stockQuantity
      , "images"
          .= [ object
                [ "src" .= image
                ]
             ]
      ]

newtype WpProductResponse = WpProductResponse
  { wpProductResponse :: Int
  }
  deriving (Show)

instance FromJSON WpProductResponse where
  parseJSON = withObject "WpProductResponse" $ \v ->
    WpProductResponse
      <$> v
      .: "id"
