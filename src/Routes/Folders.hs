{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Routes.Folders where

import           Components.Content.Header (contentHeader)
import           Components.Navbar         (navChangeAttrs)
import           Components.Shadcn.Button  (ButtonSize (DefaultSize),
                                            ButtonVariant (Destructive, Link),
                                            cnBtn, cnButton)
import           Components.Shadcn.Table   (tableCell_, tableRow_)
import           Components.Table.Simple   (TableHeader (TableHeader),
                                            simpleTable)
import           Data.Discogs.Folders      (DcArtist (dcArtistName),
                                            DcBasicInformation (dcArtists, dcGenres, dcStyles, dcThumb, dcTitle, dcYear),
                                            DcFolder (count, id),
                                            DcFolderReleaseRes (dcFolderReleases),
                                            DcFolderRes (DcFolderRes, folders),
                                            DcRelease (dcReleaseBasicInformation),
                                            foldersPath, name)
import           Data.Foldable             (Foldable (foldl'))
import           Data.List                 (intercalate)
import           Data.Text                 (Text, pack)
import           Http                      (getDcResponse)
import           Lucid                     (Html, ToHtml (toHtml), class_, div_,
                                            img_, src_)
import           Lucid.Htmx                (hxGet_, hxIndicator_, hxSwap_,
                                            hxTarget_)
import           Prelude                   hiding (id)
import           Router                    (GETRoute, PageResponse, PageRoute,
                                            getRoute)
import           Servant                   (Capture, Get, Header, (:<|>) (..),
                                            (:>))
import           Servant.HTML.Lucid        (HTML)
import           Servant.Htmx              (HXRequest)
import           State                     (AppM)

type FoldersRouter =  "folders" :> PageRoute
  :<|> "folders" :> Capture "folderId" Int :> Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse

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
              hxGet_ $ "/folders/" <> pack (show $ id f'),
              hxSwap_ "innerHTML scroll:top",
              hxTarget_ "#router-outlet",
              hxIndicator_ "#body"
              ] $ do
              tableCell_ (toHtml $ name f')
              tableCell_ (toHtml $ show $ count f')
              )
            tableHeaders = [TableHeader "Name" "", TableHeader "Count" ""]

folderContent :: Int -> GETRoute
folderContent folderId = do
  getRoute ("/folders/" <> pack fId) $ do
    (res :: (Maybe DcFolderReleaseRes)) <- getDcResponse (foldersPath <> "/" <> pack fId <> "/releases")
    pure $ do
      case res of
        Nothing -> div_ [class_ "w-full overflow-auto space-y-2"] $ do
          div_ [class_ "text-destructive-foreground"] "There might be a problem."
          cnButton Destructive DefaultSize (navChangeAttrs "/folders") "Back to folders"
        Just f -> do
          contentHeader ("Folder " <> toHtml fId) $ Just (cnButton Link DefaultSize (navChangeAttrs "/folders") "Back to folders")
          div_ [class_ "w-full overflow-auto"] $ do
            simpleTable tableHeaders $ do
              foldl' (<>) "" (fmap drawItem (dcFolderReleases f))
    where
      fId = show folderId
      tableHeaders = [TableHeader "" "", TableHeader "Title" "", TableHeader "Genres" "", TableHeader "Year" "", TableHeader "Actions" ""]
      drawItem :: DcRelease -> Html ()
      drawItem item = do
          tableRow_ [class_ " cursor-pointer"] $ do
            tableCell_ $ img_ [src_ $ pack (dcThumb basicInfo)]
            tableCell_ $ toHtml (getFullTitle (dcTitle basicInfo) (dcArtists basicInfo))
            tableCell_ $ toHtml (getGenres (dcGenres basicInfo) (dcStyles basicInfo))
            tableCell_ $ toHtml (show $ dcYear basicInfo)
            tableCell_ $ div_ [] $ do
              cnBtn "Add"
          where
            basicInfo = dcReleaseBasicInformation item

foldersRouter :: GETRoute :<|> (Int -> Maybe Text -> Maybe Text -> AppM PageResponse)
foldersRouter = getRoute "/folders" foldersContent
  :<|> folderContent
