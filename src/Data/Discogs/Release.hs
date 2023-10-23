{-# LANGUAGE OverloadedStrings #-}

module Data.Discogs.Release where

import           Data.Aeson           (FromJSON (parseJSON), withObject, (.!=),
                                       (.:), (.:?))
import           Data.Discogs.Folders (DcLabel, DcTrack, DcVideo)
import           Data.Foldable        (foldl')
import           Data.List            (zip4)
import           Data.Text            (Text, isPrefixOf, pack)
import           Data.Text.Lazy       (toStrict)
import           Data.WC.Product      (WpProduct (WpProduct))
import           Lucid                (Html, ToHtml (toHtml), a_, class_, href_,
                                       renderText, span_, table_, target_,
                                       tbody_, td_, tr_, width_)
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
  dcImages    :: Maybe [DcImage],
  dcTitle     :: String,
  dcYear      :: Int,
  dcArtists   :: String,
  dcLabels    :: [DcLabel],
  dcGenres    :: [Text],
  dcStyles    :: [Text],
  dcTracklist :: [DcTrack],
  dcVideos    :: Maybe [DcVideo]
} deriving (Show)

instance FromJSON DcRelease where
   parseJSON = withObject "DcRelease" $ \v ->
    DcRelease
      <$> v .: "id"
      <*> v .: "thumb"
      <*> v .:? "images"
      <*> v .: "title"
      <*> v .: "year"
      <*> v .: "artists_sort"
      <*> v .: "labels"
      <*> v .:? "genres" .!= [""]
      <*> v .:? "styles" .!= [""]
      <*> v .: "tracklist"
      <*> v .:? "videos"

data DcReleaseForm = DcReleaseForm {
  dcFormImageUrl      :: Text,
  dcFormCategories    :: [Int],
  dcFormTitle         :: Text,
  dcFormLabel         :: Text,
  dcFormCatNo         :: Text,
  dcFormYear          :: Int,
  dcFormStock         :: Int,
  dcFormCondition     :: Text,
  dcFormStatus        :: Text,
  dcFormPrice         :: Double,
  dcFormTrackPosition :: [Text],
  dcFormTrackTitle    :: [Text],
  dcFormTrackDuration :: [Text],
  dcFormTrackLink     :: [Text]
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

generateDescription :: DcReleaseForm -> Text
generateDescription f = "ლეიბლი - " <> dcFormLabel f <> " / " <> dcFormCatNo f <> "\nწელი - " <> (pack . show) (dcFormYear f) <> "\n"
    <> toStrict (renderText content)
    <> "\nმდგომარეობა <strong><span style=\"color: #339966;\">" <> getCustomText (dcFormCondition f) <> "(" <> dcFormCondition f <> ")</span></strong>"
  where
    getCustomText :: Text -> Text
    getCustomText inputText
      | "M" `isPrefixOf` inputText = "ახალი"
      | "NM" `isPrefixOf` inputText = "ახალივით"
      | otherwise = "კარგი"
    tracklist = trackItem <$> zip4 (dcFormTrackPosition f) (dcFormTrackTitle f) (dcFormTrackDuration f) (dcFormTrackLink f)
    trackItem :: (Text, Text, Text, Text) -> Html ()
    trackItem (pos, tname, tdur, thref) = tr_ [class_ "tracklist_track track"] $ do
      td_ [class_ "tracklist_track_pos"] (toHtml pos)
      td_ [class_ "track tracklist_track_title"] $ do
        case thref of
          "" -> span_ $ toHtml tname
          _  -> a_ [href_ thref, target_ "_blank"] (toHtml tname)
      case tdur of
        "" -> ""
        _  -> td_ [class_ "tracklist_track_duration", width_ "25"] $ toHtml tdur
    content :: Html ()
    content = table_ [class_ "playlist"] $ do
      tbody_ $ foldl' (<>) "" tracklist

generateWpPostData :: DcReleaseForm -> WpProduct
generateWpPostData f = do
  WpProduct (dcFormTitle f) "simple" (pack . show $ dcFormPrice f) (generateDescription f) (dcFormCategories f) (dcFormStatus f) True (dcFormStock f) (dcFormImageUrl f)
