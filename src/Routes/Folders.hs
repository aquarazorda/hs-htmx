{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Folders where

import           Components.Content.Header (contentHeader)
import           Components.Shadcn.Table   (tableCell_, tableRow_)
import           Components.Table.Simple   (TableHeader (TableHeader),
                                            simpleTable)
import           Data.Discogs.Folders      (DcFolder (count, id),
                                            DcFolderRes (DcFolderRes, folders),
                                            foldersPath, name)
import           Data.Foldable             (Foldable (foldl'))
import           Data.Text                 (Text, pack)
import           Http                      (getDcResponse)
import           Lucid                     (Html, ToHtml (toHtml), class_, div_)
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

type FoldersRouter = PageRoute "folders"
  :<|> "folders" :> Capture "folderId" Int :> Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse

tableHeaders :: [TableHeader]
tableHeaders = [TableHeader "Name" "", TableHeader "Count" ""]

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

folderContent :: Int -> GETRoute
folderContent folderId = do
  getRoute ("/folders/" <> pack (show folderId)) $ pure $ do
    contentHeader "Folder" Nothing
    div_ [class_ "w-full overflow-auto"] $ do
        div_ (toHtml $ show folderId)
      -- simpleTable tableHeaders $ do
      --   tableRow_ [class_ " cursor-pointer"] $ do
      --     tableCell_ (toHtml "name")
      --     tableCell_ (toHtml "count")

foldersRouter :: GETRoute :<|> (Int -> Maybe Text -> Maybe Text -> AppM PageResponse)
foldersRouter = getRoute "/folders" foldersContent
  :<|> folderContent
