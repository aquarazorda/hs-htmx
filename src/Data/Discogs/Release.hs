{-# LANGUAGE OverloadedStrings #-}

module Data.Discogs.Release where

import           Data.Aeson           (FromJSON (parseJSON), withObject, (.:),
                                       (.:?))
import           Data.Discogs.Folders (DcLabel, DcTrack, DcVideo)
import           Web.FormUrlEncoded   (FromForm, fromForm, parseAll,
                                       parseUnique)

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

data DcReleaseForm = DcReleaseForm {
  dcFormImageUrl      :: String,
  dcFormCategories    :: [Int],
  dcFormTitle         :: String,
  dcFormLabel         :: String,
  dcFormCatNo         :: String,
  dcFormYear          :: Int,
  dcFormStock         :: Int,
  dcFormCondition     :: String,
  dcFormStatus        :: String,
  dcFormPrice         :: Double,
  dcFormTrackPosition :: [String],
  dcFormTrackTitle    :: [String],
  dcFormTrackDuration :: [String],
  dcFormTrackLink     :: [String]
} deriving (Show)

instance FromForm DcReleaseForm where
  fromForm f = DcReleaseForm
    <$> parseUnique "image_url" f
    <*> parseAll "category" f
    <*> parseUnique "title" f
    <*> parseUnique "label" f
    <*> parseUnique "catno" f
    <*> parseUnique "year" f
    <*> parseUnique "stock_quantity" f
    <*> parseUnique "condition" f
    <*> parseUnique "status" f
    <*> parseUnique "price" f
    <*> parseAll "track_position" f
    <*> parseAll "track_name" f
    <*> parseAll "track_duration" f
    <*> parseAll "track_link" f
