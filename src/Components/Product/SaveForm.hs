{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Product.SaveForm where

import           Components.Navbar        (navChangeAttrs)
import           Components.Shadcn.Button (ButtonSize (ButtonSmall),
                                           ButtonVariant (ButtonDestructive, ButtonLink, ButtonSecondary),
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
import           Data.WC.Category         (WpCategory (count, name, wpCatId))
import           Http                     (getDcResponse, getWpResponse)
import           Lucid                    (Html, ToHtml (toHtml), autofocus_,
                                           blockquote_, checked_, class_, div_,
                                           fieldset_, for_, form_, id_, img_,
                                           loading_, name_, option_, p_,
                                           placeholder_, selected_, span_, src_,
                                           tabindex_, type_, value_)
import           Lucid.Base               (makeAttribute)
import           Lucid.Htmx               (hxIndicator_, hxPost_, hxSwap_,
                                           hxTarget_)
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
      cnInput [name_ "track_position", value_ $ pack $ dcTrackPosition track, class_ "hidden lg:block !w-12"]
      cnInput [name_ "track_name", value_ $ pack $ dcTrackTitle track]
      cnInput [name_ "track_duration", value_ $ pack $ dcTrackDuration track, class_ "!w-16", placeholder_ "00:00"]
      cnInput [name_ "track_link", value_ videoLink, class_ "!w-16", placeholder_ "https://youtube.com/..."]
      cnButton (Just ButtonSecondary) (Just ButtonSmall) [__ "on click get value of the previous <input/> then go to url `$it` in new window", class_ "hidden lg:block"] "Play"
      cnButton (Just ButtonDestructive) (Just ButtonSmall) [__ "on click remove closest parent <div/>"] "Remove"
      where
        videoLink = getVideoLink mVideos (pack $ dcTrackTitle track)

drawImages :: Maybe [DcImage] -> Html ()
drawImages images = div_ [
  class_ "flex flex-wrap gap-4 z-20 h-full",
  id_ "release-images"
  ] $ do
    case images of
      Nothing -> cnInput [name_ "image_url", id_ "image_input", class_ "z-20", placeholder_ "Please upload image somewhere and paste it here"]
      Just imgs -> do
        foldl' (\acc -> (acc <>) . drawImage) mempty $ zip [0..] imgs
        cnInput [name_ "image_url", id_ "image_input", value_ $ pack $ dcImageUri $ head imgs, class_ "mt-auto z-20"]
        where
          drawImage :: (Int, DcImage) -> Html ()
          drawImage (idx, image) = img_ [
            __ "on click add .active to <img/> in #release-images when it is not event.target then set value of #image_input to event.target.src",
            class_ $ "w-52 h-52 lg:w-64 lg:h-64 z-20 cursor-pointer current:opacity-50 transition-opacity" <> if idx /= 0 then " active" else "",
            src_ $ pack $ dcImageUri image,
            loading_ "lazy"
            ]

drawCategories :: [String] -> [Data.WC.Category.WpCategory] -> Html ()
drawCategories actualCats categories = div_ [class_ "flex flex-col flex-1 space-y-2"] $ do
  cnLabel [for_ "release-title"] "Category"
  fieldset_ [class_ "flex gap-1 flex-wrap h-56 overflow-y-auto scrollbar-hide border p-2 rounded-md"] $ do
    foldl' (<>) "" $ fmap catItem categories
  where
    catItem :: Data.WC.Category.WpCategory -> Html ()
    catItem c = cnToggle Outline DefaultSize (pack $ show $ wpCatId c) "category"
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
    pure $ form_ [
      hxPost_ "",
      hxSwap_ "innerHTML scroll:top",
      hxTarget_ "#router-outlet",
      hxIndicator_ "#body",
      __ "on htmx:responseError remove .hidden from #error-message",
      class_ "lg:container min-h-fit h-full overflow-hidden rounded-[0.5rem] lg:border bg-background shadow relative flex-col items-center justify-center lg:max-w-none lg:grid lg:grid-cols-2 lg:px-0"] $ do
        case res of
          Nothing -> div_ [class_ "absolute inset-0 flex flex-col gap-1 items-center justify-center"] $ do
            "There was a problem"
            cnBtn (navChangeAttrs $ "/folders/" <> folderId <> "?focusId=" <> releaseId)  "Back"
          Just r -> do
            let title = pack $ dcArtists r <> " - " <> dcTitle r
            let label = head (dcLabels r)
            let categories = map (map C.toLower) (dcStyles r <> dcGenres r)
            let backNav = tabindex_ "-1" : navChangeAttrs ("/folders/" <> folderId <> "?focusId=" <> releaseId)
            div_ [class_ "relative h-full flex-col bg-muted p-4 lg:p-10 text-white mb-3 lg:mb-0 dark:border-r lg:flex"] $ do
              div_ [class_ "absolute inset-0 bg-zinc-900"] ""
              div_ [class_ "relative z-20"] $ do
                blockquote_ [class_ "space-y-2"] $ do
                  p_ [class_ "text-lg"] "Please choose an image, add price and stock in order to save."
                  cnButton (Just ButtonLink) (Just ButtonSmall) (class_ "px-0" : backNav) "Go back."
              drawImages (dcImages r)
            div_ [class_ "flex flex-col h-full lg:p-4 justify-between gap-4"] $ do
              div_ [class_ "flex w-full flex-col justify-center space-y-6"] $ do
                div_ [class_ "flex flex-col space-y-2"] $ do
                  cnLabel [for_ "title"] "Title"
                  cnInput [name_ "title", value_ title]
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
                  cnLabel [for_ "tracklist"] "Tracklist"
                  drawTracklist (dcVideos r) (dcTracklist r)
                div_ [class_ "flex gap-2"] $ do
                  div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                      cnLabel [for_ "stock_quantity"] "Stock"
                      cnInput [name_ "stock_quantity", value_ "1", type_ "number"]
                  div_ [class_ "flex flex-col space-y-2 w-32"] $ do
                      cnLabel [for_ "condition"] "Condition"
                      cnSelect [name_ "condition"] $ do
                        option_ [value_ "M"] "Mint"
                        option_ [value_ "VG+", selected_ ""] "VG+"
                        option_ [value_ "VG"] "VG"
                        option_ [value_ "G+"] "G+"
                        option_ [value_ "G"] "G"
                  div_ [class_ "flex flex-col space-y-2 w-32"] $ do
                      cnLabel [for_ "status"] "Status"
                      cnSelect [name_ "status", value_ "publish"] $ do
                        option_ [value_ "publish"] "Publish"
                        option_ [value_ "draft"] "Draft"
                  div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                      cnLabel [for_ "price"] "Price"
                      cnInput [name_ "price", value_ "", autofocus_, placeholder_ "0.00"]
              p_ [ id_ "error-message", class_ "hidden text-red-600 self-end" ] "You're missing something."
              div_ [class_ "flex w-full"] $ do
                cnButton (Just ButtonDestructive) (Just ButtonSmall) backNav  "Back"
                cnBtn [class_ "ml-auto", type_ "submit"] "Save"

