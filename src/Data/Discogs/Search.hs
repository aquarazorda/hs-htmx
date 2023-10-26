{-# LANGUAGE OverloadedStrings #-}

module Data.Discogs.Search where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:), (.:?))
import Data.Discogs.Folders (DcPagination (..))
import Data.Text (Text)

data SearchRequestParams = SearchRequestParams
  { searchQuery :: Text
  , searchType :: Text
  , searchPerPage :: Int
  }

data DcSearchRelease = DcSearchRelease
  { dcSearchRelCountry :: Text
  , dcSearchRelYear :: Text
  , dcSearchRelCatno :: Text
  , dcSearchRelThumb :: Text
  , dcSearchRelId :: Int
  , dcSearchRelTitle :: Text
  }

instance FromJSON DcSearchRelease where
  parseJSON = withObject "DcSearchRelease" $ \v ->
    DcSearchRelease
      <$> v
      .: "country"
      <*> v
      .:? "year"
      .!= "0"
      <*> v
      .:? "catno"
      .!= ""
      <*> v
      .:? "cover_image"
      .!= ""
      <*> v
      .: "id"
      <*> v
      .: "title"

data DcSearchRes = DcSearchRes
  { dcSearchPagination :: DcPagination
  , dcSearchResults :: [DcSearchRelease]
  }

instance FromJSON DcSearchRes where
  parseJSON = withObject "DcSearchRes" $ \v ->
    DcSearchRes
      <$> v
      .: "pagination"
      <*> v
      .: "results"