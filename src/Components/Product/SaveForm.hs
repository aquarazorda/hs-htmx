{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Product.SaveForm where

import           Components.Navbar        (navChangeAttrs)
import           Components.Shadcn.Button (ButtonSize (ButtonSmall),
                                           ButtonVariant (ButtonDestructive, ButtonSecondary),
                                           cnBtn, cnButton)
import           Components.Shadcn.Input  (cnInput)
import           Components.Shadcn.Label  (cnLabel)
import           Components.Shadcn.Select (cnSelect)
import           Components.Shadcn.Toggle (ToggleSize (DefaultSize),
                                           ToggleVariant (Outline), cnToggle)
import qualified Data.Char                as C (toLower)
import           Data.Discogs.Folders     (DcLabel (dcLabelCatNo, dcLabelName),
                                           DcTrack (dcTrackDuration, dcTrackPosition, dcTrackTitle),
                                           DcVideo (dcVideoTitle, dcVideoUrl))
import           Data.Discogs.Release
import           Data.Foldable            (foldl')
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text, isInfixOf, pack, toLower)
import           Data.WC.Category         (WpCategory (count, name))
import           Http                     (getDcResponse, getWpResponse)
import           Lucid                    (Html, ToHtml (toHtml), autofocus_,
                                           blockquote_, checked_, class_, div_,
                                           footer_, for_, id_, img_, name_,
                                           option_, p_, selected_, span_, src_,
                                           tabindex_, type_, value_)
import           Lucid.Base               (makeAttribute)
import           Lucid.Hyperscript        (__)
import           State                    (AppM)

getVideoLink :: Maybe [DcVideo] -> Text -> Text
getVideoLink Nothing _ = ""
getVideoLink (Just vs) trackName = case filtered of
  Nothing -> ""
  Just v  -> pack $ dcVideoUrl v
  where
    filtered = find (\v -> toLower trackName `isInfixOf` (toLower . pack) (dcVideoTitle v)) vs

drawTracklist :: Maybe [DcVideo] -> [DcTrack] -> Html ()
drawTracklist mVideos tracks = div_ [class_ "flex flex-col space-y-2 max-h-96 overflow-y-auto scrollbar-hide"] $ do
  foldl' (<>) "" $ fmap drawTrack tracks
  where
    drawTrack :: DcTrack -> Html ()
    drawTrack track = div_ [class_ "flex space-x-2 items-center" ] $ do
      cnInput [value_ $ pack $ dcTrackPosition track, class_ "!w-12"]
      cnInput [value_ $ pack $ dcTrackTitle track]
      cnInput [value_ $ pack $ dcTrackDuration track, class_ "!w-16"]
      cnInput [value_ videoLink, class_ "!w-16"]
      cnButton (Just ButtonSecondary) (Just ButtonSmall) [__ "on click get value of the previous <input/> then go to url `$it` in new window"] "Play"
      cnButton (Just ButtonDestructive) (Just ButtonSmall) [__ "on click remove closest parent <div/>"] "Remove"
      where
        videoLink = getVideoLink mVideos (pack $ dcTrackTitle track)

drawImages :: [DcImage] -> Html ()
drawImages images = div_ [
  class_ "flex flex-wrap gap-4 z-20",
  id_ "release-images",
  __ "on click add .active to <img/> in #release-images when it is not event.target"
  ] $ do
  foldl' (<>) "" $ fmap drawImage images
  where
    drawImage :: DcImage -> Html ()
    drawImage image = img_ [class_ "w-64 cursor-pointer current:opacity-50 transition-opacity", src_ $ pack $ dcImageUri image]

drawCategories :: [String] -> [Data.WC.Category.WpCategory] -> Html ()
drawCategories actualCats categories = div_ [class_ "flex flex-col flex-1 space-y-2"] $ do
  cnLabel [for_ "release-title"] "Category"
  div_ [class_ "flex gap-1 flex-wrap h-56 overflow-y-auto scrollbar-hide border p-2 rounded-md"] $ do
    foldl' (<>) "" $ fmap catItem categories
  where
    catItem :: Data.WC.Category.WpCategory -> Html ()
    catItem c = cnToggle Outline DefaultSize name' "category"
      ([id_ name', class_ "flex-1 whitespace-nowrap"
        , makeAttribute "data-state" $ if isChecked  then "on" else "off"
        , __ "get the (innerHTML of first <span/> in me) as an Int if my @data-state is on decrement it else increment it end put it into <span/> in me"
      ]
      <> ([checked_ | isChecked]))
      $ do
        div_ [class_ "flex gap-2"] $ do
          p_ $ toHtml (name c)
          span_ $ toHtml (show $ if isChecked then count c + 1 else count c)
        where
          name' = pack $ name c
          isChecked = foldl' (\acc cat -> acc || (toLower name' `isInfixOf` pack cat)) False actualCats

productSaveForm :: Text -> Text -> AppM (Html ())
productSaveForm folderId releaseId = do
    (res :: Maybe DcRelease) <- getDcResponse $ "/releases/" <> releaseId
    (catRes :: Maybe [Data.WC.Category.WpCategory]) <- getWpResponse "/products/categories?per_page=100&orderby=count&order=desc"
    pure $ div_ [class_ "container h-full overflow-hidden rounded-[0.5rem] border bg-background shadow relative hidden flex-col items-center justify-center md:grid lg:max-w-none lg:grid-cols-2 lg:px-0"] $ do
        case res of
          Nothing -> div_ [class_ "absolute inset-0 flex flex-col gap-1 items-center justify-center"] $ do
            "There was a problem"
            cnBtn (navChangeAttrs $ "/folders/" <> folderId <> "?focusId=" <> releaseId)  "Back"
          Just r -> do
            let title = pack $ dcArtists r <> " - " <> dcTitle r
            let label = head (dcLabels r)
            let categories = map (map C.toLower) (dcStyles r <> dcGenres r)
            div_ [class_ "relative hidden h-full flex-col bg-muted p-10 space-y-6 text-white dark:border-r lg:flex"] $ do
              div_ [class_ "absolute inset-0 bg-zinc-900"] ""
              div_ [class_ "relative z-20"] $ do
                blockquote_ [class_ "space-y-2"] $ do
                  p_ [class_ "text-lg"] "Please choose an image, add price and stock in order to save."
                  footer_ [class_ "text-sm"] "You can always edit release later."
              drawImages $ dcImages r
            div_ [class_ "flex flex-col h-full p-4"] $ do
              div_ [class_ "flex w-full flex-col justify-center space-y-6"] $ do
                div_ [class_ "flex flex-col space-y-2"] $ do
                  cnLabel [for_ "release-title"] "Title"
                  cnInput [value_ title]
                div_ [class_ "flex gap-2"] $ do
                  div_ [class_ "flex flex-col flex-1 space-y-2"] $ do
                    cnLabel [for_ "label"] "Label"
                    cnInput [name_ "label", value_ $ pack (dcLabelName label)]
                  div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                    cnLabel [for_ "catno"] "Cat#"
                    cnInput [name_ "catno", value_ $ pack (dcLabelCatNo label)]
                  div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                        cnLabel [for_ "year"] "Year"
                        cnInput [name_ "year", value_ $ pack $ show (dcYear r), type_ "number"]
                drawCategories categories $ fromMaybe [] catRes
                div_ [class_ "flex flex-col space-y-2"] $ do
                  cnLabel [for_ "release-tracklist"] "Tracklist"
                  drawTracklist (dcVideos r) (dcTracklist r)
                div_ [class_ "flex gap-2"] $ do
                  div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                      cnLabel [for_ "stock_quantity"] "Stock"
                      cnInput [name_ "stock_quantity", value_ "1", type_ "number"]
                  div_ [class_ "flex flex-col space-y-2 w-32"] $ do
                      cnLabel [for_ "condition"] "Condition"
                      cnSelect [name_ "condition"] $ do
                        option_ [value_ "mint"] "Mint"
                        option_ [value_ "vg+", selected_ ""] "VG+"
                        option_ [value_ "vg"] "VG"
                        option_ [value_ "g+"] "G+"
                        option_ [value_ "g"] "G"
                  div_ [class_ "flex flex-col space-y-2 w-32"] $ do
                      cnLabel [for_ "status"] "Status"
                      cnSelect [name_ "status", value_ "publish"] $ do
                        option_ [value_ "publish"] "Publish"
                        option_ [value_ "draft"] "Draft"
                  div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                      cnLabel [for_ "price"] "Price"
                      cnInput [name_ "price", value_ "", type_ "number", autofocus_]
              div_ [class_ "flex w-full mt-auto"] $ do
                cnButton (Just ButtonDestructive) (Just ButtonSmall)
                  (navChangeAttrs ("/folders/" <> folderId <> "?focusId=" <> releaseId) <> [tabindex_ "-1"])  "Back"
                cnBtn [class_ "ml-auto", type_ "submit"] "Save"

