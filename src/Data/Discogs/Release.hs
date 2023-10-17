{-# LANGUAGE OverloadedStrings #-}

module Data.Discogs.Release where

import           Data.Aeson           (FromJSON (parseJSON), withObject, (.:),
                                       (.:?))
import           Data.Discogs.Folders (DcLabel, DcTrack, DcVideo)

data DcImage = DcImage {
  dcImageUri    :: String,
  dcImageWidth  :: Int,
  dcImageHeight :: Int
} deriving (Show)

instance FromJSON DcImage where
    parseJSON = withObject "DcImage" $ \v ->
      DcImage
        <$> v .: "uri"
        <*> v .: "width"
        <*> v .: "height"

data DcRelease = DcRelease {
  dcId        :: Int,
  dcThumb     :: String,
  dcImages    :: [DcImage],
  dcTitle     :: String,
  dcYear      :: Int,
  dcArtists   :: String,
  dcLabels    :: [DcLabel],
  dcGenres    :: [String],
  dcStyles    :: [String],
  dcTracklist :: [DcTrack],
  dcVideos    :: Maybe [DcVideo]
} deriving (Show)

instance FromJSON DcRelease where
   parseJSON = withObject "DcRelease" $ \v ->
    DcRelease
      <$> v .: "id"
      <*> v .: "thumb"
      <*> v .: "images"
      <*> v .: "title"
      <*> v .: "year"
      <*> v .: "artists_sort"
      <*> v .: "labels"
      <*> v .: "genres"
      <*> v .: "styles"
      <*> v .: "tracklist"
      <*> v .:? "videos"
