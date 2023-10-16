{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Routes.Folders where

import           Components.Content.Header   (contentHeader)
import           Components.Icons            (arrowLeft, arrowRight,
                                              doubleArrowLeft, doubleArrowRight)
import           Components.Navbar           (navChangeAttrs)
import           Components.Product.SaveForm (productSaveForm)
import           Components.Shadcn.Button    (ButtonSize (DefaultSize),
                                              ButtonVariant (Destructive, Link, Outline),
                                              cnBtn, cnButton, disableWhen)
import           Components.Shadcn.Table     (tableCell_, tableRow_)
import           Components.Table.Simple     (TableHeader (TableHeader),
                                              simpleTable)
import           Data.Discogs.Folders        (DcArtist (dcArtistName),
                                              DcBasicInformation (dcArtists, dcGenres, dcId, dcStyles, dcThumb, dcTitle, dcYear),
                                              DcFolder (dcFolderCount, dcFolderId, dcFolderName),
                                              DcFolderReleaseRes (dcFolderReleasePagination, dcFolderReleases),
                                              DcFolderRes (DcFolderRes, folders),
                                              DcPagination (dcPaginationPage, dcPaginationPages),
                                              DcRelease (dcReleaseBasicInformation),
                                              foldersPath)
import           Data.Foldable               (Foldable (foldl'))
import           Data.List                   (intercalate)
import           Data.Text                   (Text, pack)
import           Http                        (getDcResponse)
import           Lucid                       (Html, ToHtml (toHtml), class_,
                                              disabled_, div_, img_, span_,
                                              src_)
import           Lucid.Htmx                  (hxGet_, hxIndicator_, hxSwap_,
                                              hxTarget_)
import           Router                      (GETRoute, PageResponse, PageRoute,
                                              getRoute)
import           Servant                     (Capture, Get, Header, QueryParam,
                                              (:<|>) (..), (:>))
import           Servant.HTML.Lucid          (HTML)
import           Servant.Htmx                (HXRequest)
import           State                       (AppM)

type FoldersRouter =  "folders" :> PageRoute
  :<|> "folders" :> Capture "folderId" Int :> QueryParam "page" Int :> Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse
  :<|> "folders" :> Capture "folderId" Int :> Capture "releaseId" Int :> Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse

getFullTitle :: String -> [DcArtist] -> String
getFullTitle title artists = intercalate " & " (map dcArtistName artists) <> " - " <> title

getGenres :: [String] -> [String] -> String
getGenres genres styles = intercalate ", " (genres ++ styles)

foldersContent :: AppM (Html ())
foldersContent = do
  (mFolders :: Maybe DcFolderRes) <- getDcResponse foldersPath
  pure $ do
    contentHeader "Folders" Nothing
    div_ [class_ "w-full overflow-auto"] $ do
      simpleTable tableHeaders $
        case mFolders of
          Just (DcFolderRes { folders = f }) -> foldl' (<>) "" (drawItem f)
          _                                  -> tableRow_ ""
          where
            drawItem = fmap (\f' -> tableRow_ [
              class_ " cursor-pointer",
              hxGet_ $ "/folders/" <> pack (show $ dcFolderId f'),
              hxSwap_ "innerHTML scroll:top",
              hxTarget_ "#router-outlet",
              hxIndicator_ "#body"
              ] $ do
              tableCell_ (toHtml $ dcFolderName f')
              tableCell_ (toHtml $ show $ dcFolderCount f')
              )
            tableHeaders = [TableHeader "Name" "", TableHeader "Count" ""]

type FolderContent = Int -> Maybe Int -> GETRoute

folderContent :: FolderContent
folderContent folderId page = do
  getRoute ("/folders/" <> pack fId) $ do
    (res :: (Maybe DcFolderReleaseRes)) <- getDcResponse (foldersPath <> "/" <> pack fId <> "/releases")
    pure $ do
      case res of
        Nothing -> div_ [class_ "w-full overflow-auto space-y-2"] $ do
          div_ [class_ "text-destructive-foreground"] "There might be a problem."
          cnButton (Just Destructive) (Just DefaultSize) (navChangeAttrs "/folders") "Back to folders"
        Just f -> do
          let pagination = dcFolderReleasePagination f
          let p = maybe 0 id page
          contentHeader ("Folder " <> toHtml fId) $ Just (cnButton (Just Link) (Just DefaultSize) (navChangeAttrs "/folders") "Back to folders")
          div_ [class_ "w-full overflow-auto space-y-4"] $ do
            simpleTable tableHeaders $ do
              foldl' (<>) "" (fmap drawItem (dcFolderReleases f))
            div_ [class_ "flex justify-end px-2"] $ do
              div_ [class_ "flex items-center space-x-6 lg:space-x-8"] $ do
                div_ [class_ "flex w-[100px] items-center justify-center text-sm font-medium"] $ do
                  "Page " <> toHtml (show $ dcPaginationPage pagination) <> " of " <> toHtml (show $ dcPaginationPages pagination)
                div_ [class_ "flex items-center space-x-2"] $ do
                  cnButton (Just Outline) Nothing (disableWhen (p < 2) [class_ "hidden h-8 w-8 p-0 lg:flex justify-center"]) $ do
                    span_ [class_ "sr-only"] "Go to first page"
                    doubleArrowLeft [class_ "h-4 w-4"]
                  cnButton (Just Outline) Nothing (disableWhen (p < 2) [class_ "h-8 w-8 p-0 justify-center"]) $ do
                    span_ [class_ "sr-only"] "Go to previous page"
                    arrowLeft [class_ "h-4 w-4"]
                  cnButton (Just Outline) Nothing (disableWhen (p >= dcPaginationPages pagination) [class_ "h-8 w-8 p-0 justify-center"]) $ do
                    span_ [class_ "sr-only"] "Go to next page"
                    arrowRight [class_ "h-4 w-4"]
                  cnButton (Just Outline) Nothing (disableWhen (p >= dcPaginationPages pagination) [class_ "hidden h-8 w-8 p-0 lg:flex justify-center"]) $ do
                    span_ [class_ "sr-only"] "Go to last page"
                    doubleArrowRight [class_ "h-4 w-4"]
    where
      fId = show folderId
      tableHeaders = [TableHeader "" "", TableHeader "Title" "", TableHeader "Genres" "", TableHeader "Year" "", TableHeader "Actions" ""]
      drawItem :: DcRelease -> Html ()
      drawItem item = do
          tableRow_ [class_ " cursor-pointer"] $ do
            tableCell_ $ img_ [src_ $ pack (dcThumb basicInfo), class_ "w-20 h-20"]
            tableCell_ $ toHtml (getFullTitle (dcTitle basicInfo) (dcArtists basicInfo))
            tableCell_ $ toHtml (getGenres (dcGenres basicInfo) (dcStyles basicInfo))
            tableCell_ $ toHtml (show $ dcYear basicInfo)
            tableCell_ $ div_ [] $ do
              cnBtn (navChangeAttrs releasePath) "Add"
          where
            basicInfo = dcReleaseBasicInformation item
            releasePath = "/folders/" <> pack fId <> "/" <> pack (show $ dcId basicInfo)

type ReleaseContent = Int -> Int -> GETRoute

releaseContent :: ReleaseContent
releaseContent folderId releaseId = getRoute ("/folders/" <> pack (show folderId) <> "/" <> pack (show releaseId))
  $ pure productSaveForm

foldersRouter :: GETRoute
  :<|> FolderContent
  :<|> ReleaseContent
foldersRouter = getRoute "/folders" foldersContent
  :<|> folderContent
  :<|> releaseContent
