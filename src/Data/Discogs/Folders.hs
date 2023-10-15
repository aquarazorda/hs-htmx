{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Discogs.Folders where

import           Data.Aeson   (FromJSON (parseJSON), withObject, (.:))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data DcFolder = DcFolder {
  id          :: Int,
  name        :: String,
  count       :: Int,
  resourceUrl :: String
} deriving (Show)

newtype DcFolderRes = DcFolderRes {
  folders :: [DcFolder]
} deriving (Generic, Show)

instance FromJSON DcFolder where
  parseJSON = withObject "DcFolder" $ \v ->
    DcFolder
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "count"
      <*> v .: "resource_url"

instance FromJSON DcFolderRes

foldersPath :: Text
foldersPath = "/users/MoreviTBS/collection/folders"
