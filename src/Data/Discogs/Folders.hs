{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Discogs.Folders where

import           Data.Aeson   (FromJSON (parseJSON), withObject, (.:))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

foldersPath :: Text
foldersPath = "/users/MoreviTBS/collection/folders"

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

data DcPagination = DcPagination {
  dcPaginationPerPage :: Int,
  dcPaginationItems   :: Int,
  dcPaginationPage    :: Int,
  dcPaginationPages   :: Int
} deriving (Show)

instance FromJSON DcPagination where
  parseJSON = withObject "DcPagination" $ \v ->
    DcPagination
      <$> v .: "per_page"
      <*> v .: "items"
      <*> v .: "page"
      <*> v .: "pages"

data DcArtist = DcArtist {
  dcArtistId   :: Int,
  dcArtistName :: String
} deriving (Show)

instance FromJSON DcArtist where
  parseJSON = withObject "DcArtist" $ \v ->
    DcArtist
      <$> v .: "id"
      <*> v .: "name"

data DcLabel = DcLabel {
  dcLabelId    :: Int,
  dcLabelName  :: String,
  dcLabelCatNo :: String
} deriving (Show)

instance FromJSON DcLabel where
  parseJSON = withObject "DcLabel" $ \v ->
    DcLabel
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "catno"

data DcNote = DcNote {
  dcFieldId :: Int,
  dcValue   :: String
} deriving (Show)

instance FromJSON DcNote where
  parseJSON = withObject "DcNote" $ \v ->
    DcNote
      <$> v .: "field_id"
      <*> v .: "value"

data DcBasicInformation = DcBasicInformation {
  dcThumb      :: String,
  dcCoverImage :: String,
  dcTitle      :: String,
  dcYear       :: Int,
  dcArtists    :: [DcArtist],
  dcLabels     :: [DcLabel],
  dcGenres     :: [String],
  dcStyles     :: [String]
} deriving (Show)

instance FromJSON DcBasicInformation where
  parseJSON = withObject "DcBasicInformation" $ \v ->
    DcBasicInformation
      <$> v .: "thumb"
      <*> v .: "cover_image"
      <*> v .: "title"
      <*> v .: "year"
      <*> v .: "artists"
      <*> v .: "labels"
      <*> v .: "genres"
      <*> v .: "styles"

data DcRelease = DcRelease {
  dcReleaseId               :: Int,
  dcReleaseRating           :: Int,
  dcReleaseBasicInformation :: DcBasicInformation,
  dcFolderId                :: Int,
  dcNotes                   :: [DcNote]
} deriving (Show)

instance FromJSON DcRelease where
  parseJSON = withObject "DcRelease" $ \v ->
    DcRelease
      <$> v .: "id"
      <*> v .: "rating"
      <*> v .: "basic_information"
      <*> v .: "folder_id"
      <*> v .: "notes"

data DcFolderReleaseRes = DcFolderReleaseRes {
  dcFolderReleasePagination :: DcPagination,
  dcFolderReleases          :: [DcRelease]
} deriving (Show)

instance FromJSON DcFolderReleaseRes where
  parseJSON = withObject "DcFolderReleaseRes" $ \v ->
    DcFolderReleaseRes
      <$> v .: "pagination"
      <*> v .: "releases"
