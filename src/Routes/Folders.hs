{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Folders where

import Components.Content.Header (contentHeader)
import Components.Icons
  ( arrowLeft
  , arrowRight
  , doubleArrowLeft
  , doubleArrowRight
  )
import Components.Shadcn.Button
  ( ButtonSize (ButtonDefaultSize)
  , ButtonVariant (ButtonDestructive, ButtonLink, ButtonOutline)
  , cnBtn
  , cnButton
  , disableWhen
  )
import Components.Shadcn.Table (tableCell_, tableRow_)
import Components.Table.Simple
  ( TableHeader (TableHeader)
  , simpleTable
  )
import Data.Discogs.Folders
  ( DcBasicInformation (dcArtists, dcGenres, dcId, dcStyles, dcThumb, dcTitle, dcYear)
  , DcFolder (dcFolderCount, dcFolderId, dcFolderName)
  , DcFolderReleaseRes (dcFolderReleasePagination, dcFolderReleases)
  , DcFolderRes (DcFolderRes, folders)
  , DcNote (dcFieldId, dcValue)
  , DcPagination (dcPaginationPage, dcPaginationPages)
  , DcRelease (dcNotes, dcReleaseBasicInformation)
  , foldersPath
  , getFullTitle
  )
import Data.Foldable (Foldable (foldl'))
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat, pack)
import Htmx (navChangeAttrs)
import Http (getDcResponse)
import Lucid
  ( Html
  , ToHtml (toHtml)
  , class_
  , div_
  , id_
  , img_
  , loading_
  , span_
  , src_
  )
import Lucid.Htmx (hxHeaders_)
import Lucid.Hyperscript (withAutoFocus)
import Router.Helpers (GenericResponse, getRoute, toUrlPiece_)
import Servant (fieldLink)
import Servant.Server.Generic (AsServerT)
import State (AppM)
import Types.Api (FoldersApi (FoldersApi, getFolder, getFolders), ProductsApi (getRelease))
import Utils
  ( extractContentInParentheses
  , extractFirstNumToDouble
  )
import Prelude hiding (concat, putStrLn)

foldersApi :: FoldersApi (AsServerT AppM)
foldersApi =
  FoldersApi
    { getFolders = getRoute "/folders" foldersContent
    , getFolder = folderContent
    }

getGenres :: [String] -> [String] -> String
getGenres genres styles = intercalate ", " (genres ++ styles)

foldersContent :: AppM (Html ())
foldersContent = do
  (mFolders :: Maybe DcFolderRes) <- getDcResponse foldersPath
  pure $ do
    contentHeader "Folders" Nothing
    div_ [class_ "w-full overflow-auto"] $ do
      simpleTable tableHeaders
        $ case mFolders of
          Just (DcFolderRes {folders = f}) -> foldl' (<>) "" (drawItem f)
          _ -> tableRow_ ""
 where
  drawItem =
    fmap
      ( \f' -> tableRow_
          ( class_ " cursor-pointer" : navChangeAttrs (toUrlPiece_ $ fieldLink getFolder (dcFolderId f') (Just 1))
          )
          $ do
            tableCell_ (toHtml $ dcFolderName f')
            tableCell_ (toHtml $ show $ dcFolderCount f')
      )
  tableHeaders = [TableHeader "Name" "", TableHeader "Count" ""]

type FolderContent = Int -> Maybe Int -> GenericResponse

folderContent :: FolderContent
folderContent folderId page = do
  getRoute (toUrlPiece_ $ fieldLink getFolder folderId page) $ do
    (res :: (Maybe DcFolderReleaseRes)) <-
      getDcResponse
        $ concat [foldersPath, "/", pack fId, "/releases", "?page=", pack (show p), "&sort=added&sort_order=desc"]
    pure $ do
      case res of
        Nothing -> div_ [class_ "w-full overflow-auto space-y-2"] $ do
          div_ [class_ "text-destructive-foreground"] "There might be a problem."
          cnButton (Just ButtonDestructive) (Just ButtonDefaultSize) (navChangeAttrs "/folders") "Back to folders"
        Just f -> do
          let pagination = dcFolderReleasePagination f
          contentHeader ("Folder " <> toHtml fId) $ Just (cnButton (Just ButtonLink) (Just ButtonDefaultSize) (navChangeAttrs "/folders") "Back to folders")
          div_ [class_ "w-full overflow-auto space-y-4", withAutoFocus] $ do
            simpleTable tableHeaders $ do
              foldl' (<>) "" (fmap drawItem (dcFolderReleases f))
            div_ [class_ "flex justify-end px-2"] $ do
              div_ [class_ "flex items-center space-x-6 lg:space-x-8"] $ do
                div_ [class_ "flex w-[100px] items-center justify-center text-sm font-medium"] $ do
                  "Page " <> toHtml (show $ dcPaginationPage pagination) <> " of " <> toHtml (show $ dcPaginationPages pagination)
                div_ [class_ "flex items-center space-x-2"] $ do
                  cnButton
                    (Just ButtonOutline)
                    Nothing
                    ( disableWhen (p < 2)
                        $ class_ "hidden h-8 w-8 p-0 lg:flex justify-center"
                        : navChangeAttrs ("/folders/" <> pack fId <> "?page=1")
                    )
                    $ do
                      span_ [class_ "sr-only"] "Go to first page"
                      doubleArrowLeft [class_ "h-4 w-4"]
                  cnButton
                    (Just ButtonOutline)
                    Nothing
                    ( disableWhen (p < 2)
                        $ class_ "h-8 w-8 p-0 justify-center"
                        : navChangeAttrs ("/folders/" <> pack fId <> "?page=" <> pack (show $ p - 1))
                    )
                    $ do
                      span_ [class_ "sr-only"] "Go to previous page"
                      arrowLeft [class_ "h-4 w-4"]
                  cnButton
                    (Just ButtonOutline)
                    Nothing
                    ( disableWhen (p >= dcPaginationPages pagination)
                        $ class_ "h-8 w-8 p-0 justify-center"
                        : navChangeAttrs ("/folders/" <> pack fId <> "?page=" <> pack (show $ p + 1))
                    )
                    $ do
                      span_ [class_ "sr-only"] "Go to next page"
                      arrowRight [class_ "h-4 w-4"]
                  cnButton
                    (Just ButtonOutline)
                    Nothing
                    ( disableWhen (p >= dcPaginationPages pagination)
                        $ class_ "hidden h-8 w-8 p-0 lg:flex justify-center"
                        : navChangeAttrs ("/folders/" <> pack fId <> "?page=" <> pack (show $ dcPaginationPages pagination))
                    )
                    $ do
                      span_ [class_ "sr-only"] "Go to last page"
                      doubleArrowRight [class_ "h-4 w-4"]
 where
  p = fromMaybe 1 page
  fId = show folderId
  tableHeaders = [TableHeader "" "", TableHeader "Title" "", TableHeader "Genres" "", TableHeader "Year" "", TableHeader "Actions" ""]
  drawItem :: DcRelease -> Html ()
  drawItem item = do
    tableRow_ ([class_ "cursor-pointer", id_ relId] <> relNav) $ do
      tableCell_ $ img_ [src_ $ pack (dcThumb basicInfo), class_ "w-20 h-20", loading_ "lazy"]
      tableCell_ [class_ "max-w-md truncate"] $ toHtml (getFullTitle (dcTitle basicInfo) (dcArtists basicInfo))
      tableCell_ [class_ "max-w-md truncate"] $ toHtml (getGenres (dcGenres basicInfo) (dcStyles basicInfo))
      tableCell_ $ toHtml (show $ dcYear basicInfo)
      tableCell_ $ div_ [] $ do
        cnBtn relNav "Add"
   where
    basicInfo = dcReleaseBasicInformation item
    relId = pack $ show (dcId basicInfo)
    releasePath =
      toUrlPiece_
        $ fieldLink
          getRelease
          (dcId basicInfo)
          (Just $ (pack . show . getItemPrice) item)
          (Just $ getItemCondition item)
          (Just folderId)
          Nothing
          page
    relNav = hxHeaders_ ("{\"page\": \"" <> pack (show p) <> "\"}") : navChangeAttrs releasePath

getItemPrice :: DcRelease -> Double
getItemPrice dcRelease = maybe 0.00 (decrement . extractFirstNumToDouble . dcValue) (find (\i -> dcFieldId i == 3) =<< dcNotes dcRelease)
 where
  decrement d = if d > 0 then d - 0.01 else d

priceAsQueryString :: Double -> Text
priceAsQueryString p = "?price=" <> pack (show p)

getItemCondition :: DcRelease -> Text
getItemCondition dcRelease = maybe "VG+" (extractContentInParentheses . pack . dcValue) (find (\i -> dcFieldId i == 1) =<< dcNotes dcRelease)
