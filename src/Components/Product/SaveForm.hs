{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Product.SaveForm where

import Components.Shadcn.Button
  ( ButtonSize (ButtonSmall)
  , ButtonVariant (ButtonDestructive, ButtonLink, ButtonSecondary)
  , cnBtn
  , cnButton
  )
import Components.Shadcn.Input (cnInput)
import Components.Shadcn.Label (cnLabel)
import Components.Shadcn.Select (cnSelect)
import Components.Spinner (spinner)
import Data.Discogs.Folders
  ( DcLabel (dcLabelCatNo, dcLabelName)
  , DcTrack (dcTrackDuration, dcTrackPosition, dcTrackTitle)
  , DcVideo (dcVideoTitle, dcVideoUrl)
  )
import Data.Discogs.Release
  ( DcImage (dcImageUri)
  , DcRelease
    ( dcArtists
    , dcGenres
    , dcImages
    , dcLabels
    , dcStyles
    , dcTitle
    , dcTracklist
    , dcVideos
    , dcYear
    )
  )
import Data.Foldable (foldl')
import Data.List (find)
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , toLower
  )
import Htmx (hxDisinherit_, navChangeAttrs)
import Http (getDcResponse)
import Lucid
  ( Html
  , blockquote_
  , class_
  , div_
  , for_
  , form_
  , id_
  , img_
  , loading_
  , name_
  , option_
  , p_
  , placeholder_
  , selected_
  , src_
  , tabindex_
  , type_
  , value_
  )
import Lucid.Htmx
  ( hxGet_
  , hxIndicator_
  , hxPost_
  , hxSwap_
  , hxTarget_
  , hxTrigger_
  )
import Lucid.Hyperscript (__)
import Router.Helpers (toUrlPiece_)
import Servant (fieldLink)
import State (AppM)
import Types.Api (AddProductApi (addProduct), FoldersApi (getFolder), ProductsApi (postRelease))
import Utils (concatAsPrintable)

selectedOption_ :: Text -> Text -> Html () -> Html ()
selectedOption_ selectedVal val = option_ (if val == selectedVal then [value_ val, selected_ ""] else [value_ val])

getVideoLink :: Maybe [DcVideo] -> Text -> Text
getVideoLink Nothing _ = ""
getVideoLink (Just vs) trackName = case filtered of
  Nothing -> ""
  Just v -> pack $ dcVideoUrl v
 where
  filtered = find (\v -> toLower trackName `isInfixOf` (toLower . pack) (dcVideoTitle v)) vs

drawTracklist :: Maybe [DcVideo] -> [DcTrack] -> Html ()
drawTracklist mVideos tracks = div_ [class_ "flex flex-col space-y-2 max-h-96 overflow-y-auto scrollbar-hide"] $ do
  foldl' (<>) "" $ fmap drawTrack tracks
 where
  drawTrack :: DcTrack -> Html ()
  drawTrack track = div_ [class_ "flex space-x-2 items-center"] $ do
    cnInput [name_ "track_position", value_ $ pack $ dcTrackPosition track, class_ "hidden lg:block !w-12"]
    cnInput [name_ "track_name", value_ $ pack $ dcTrackTitle track]
    cnInput [name_ "track_duration", value_ $ pack $ dcTrackDuration track, class_ "!w-16", placeholder_ "00:00"]
    cnInput [name_ "track_link", value_ videoLink, class_ "!w-16", placeholder_ "Link"]
    cnButton (Just ButtonSecondary) (Just ButtonSmall) [__ "on click get value of the previous <input/> then go to url `$it` in new window", class_ "hidden lg:block"] "Play"
    cnButton (Just ButtonDestructive) (Just ButtonSmall) [__ "on click remove closest parent <div/>"] "Remove"
   where
    videoLink = getVideoLink mVideos (pack $ dcTrackTitle track)

drawImages :: Maybe [DcImage] -> Html ()
drawImages images = div_
  [ class_ "flex flex-wrap gap-4 z-20 h-full"
  , id_ "release-images"
  ]
  $ do
    case images of
      Nothing -> cnInput [name_ "image_url", id_ "image_input", class_ "z-20", placeholder_ "Please upload image somewhere and paste it here"]
      Just imgs -> do
        foldl' (\acc -> (acc <>) . drawImage) mempty $ zip [0 ..] imgs
        cnInput [name_ "image_url", id_ "image_input", value_ $ pack $ dcImageUri (head imgs), class_ "mt-auto z-20"]
       where
        drawImage :: (Int, DcImage) -> Html ()
        drawImage (idx, image) =
          img_
            [ __ "on click add .active to <img/> in #release-images when it is not event.target then set value of #image_input to event.target.src"
            , class_ $ "w-40 h-40 lg:w-64 lg:h-64 z-20 cursor-pointer current:opacity-50 transition-opacity" <> if idx /= 0 then " active" else ""
            , src_ $ pack $ dcImageUri image
            , loading_ "lazy"
            ]

productSaveForm :: Text -> Int -> Text -> Maybe Int -> Maybe Text -> Int -> AppM (Html ())
productSaveForm price page condition folderId queryString releaseId = do
  (res :: Maybe DcRelease) <- getDcResponse ("/releases/" <> pack (show releaseId))
  pure
    $ form_
      [ hxPost_ $ toUrlPiece_ $ fieldLink postRelease folderId (Just page) queryString
      , hxSwap_ "innerHTML scroll:top"
      , hxTarget_ "#router-outlet"
      , hxDisinherit_ "*"
      , hxIndicator_ "#body"
      , __
          $ "init set $focusId to "
          <> pack (show releaseId)
          <> " then on htmx:responseError remove .hidden from #error-message then remove @disabled from #submit-button\
             \ then on htmx:beforeRequest add @disabled to #submit-button"
      , class_ "lg:container min-h-fit h-full overflow-hidden rounded-[0.5rem] lg:border bg-background shadow relative flex-col items-center justify-center lg:max-w-none lg:grid lg:grid-cols-5 lg:px-0"
      ]
    $ do
      let backNav =
            [tabindex_ "-1"]
              <> navChangeAttrs
                ( case folderId of
                    Nothing -> toUrlPiece_ $ fieldLink addProduct queryString
                    Just fId -> toUrlPiece_ $ fieldLink getFolder fId (Just page)
                )
      case res of
        Nothing -> div_ [class_ "absolute inset-0 flex flex-col gap-1 items-center justify-center"] $ do
          "There was a problem"
          cnBtn backNav "Back"
        Just r -> do
          let title = pack $ dcArtists r <> " - " <> dcTitle r
          let label = head (dcLabels r)
          let categories = concatAsPrintable $ dcStyles r <> dcGenres r
          div_ [class_ "relative h-full flex-col bg-muted p-4 lg:p-10 mb-3 lg:mb-0 dark:border-r lg:flex col-span-2"] $ do
            div_ [class_ "absolute inset-0 bg-zinc-900"] ""
            div_ [class_ "relative z-20"] $ do
              blockquote_ [class_ "space-y-2"] $ do
                p_ [class_ "lg:text-lg text-white"] "Please choose an image, add price and stock in order to save."
                cnButton (Just ButtonLink) (Just ButtonSmall) ([class_ "px-0 text-white"] <> backNav) "Go back."
            drawImages (dcImages r)
          div_ [class_ "flex flex-col h-full p-4 justify-between gap-8 col-span-3"] $ do
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
              div_
                [ hxTrigger_ "load"
                , hxSwap_ "innerHTML"
                , hxGet_ "/categories/wp"
                , hxDisinherit_ "*"
                , __
                    $ "on htmx:afterSwap set :cats to "
                    <> categories
                    <> " then repeat for cat in (<button/> in <fieldset/>)\
                       \ if :cats.includes((@data-value of cat).trim()) then\
                       \ increment the (innerHTML of first <span/> in cat) then\
                       \ add @data-state=on to cat then\
                       \ add @checked=true to the first <input/> in cat end end"
                ]
                $ do
                  spinner "categories-loader" "flex h-36"
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
                    selectedOption_ condition "M" "Mint"
                    selectedOption_ condition "NM" "Near Mint"
                    selectedOption_ condition "VG+" "VG+"
                    selectedOption_ condition "VG" "VG"
                    selectedOption_ condition "G+" "G+"
                    selectedOption_ condition "G" "G"
                div_ [class_ "flex flex-col space-y-2 w-32"] $ do
                  cnLabel [for_ "status"] "Status"
                  cnSelect [name_ "status", value_ "publish"] $ do
                    option_ [value_ "publish"] "Publish"
                    option_ [value_ "draft"] "Draft"
                div_ [class_ "flex flex-col space-y-2 w-24"] $ do
                  cnLabel [for_ "price"] "Price"
                  cnInput [name_ "price", value_ price, placeholder_ "0.00"]
            p_ [id_ "error-message", class_ "hidden text-red-600 self-end"] "You're missing something."
            div_ [class_ "flex w-full"] $ do
              cnButton (Just ButtonDestructive) (Just ButtonSmall) backNav "Back"
              cnBtn [id_ "submit-button", class_ "ml-auto", type_ "submit"] "Save"
