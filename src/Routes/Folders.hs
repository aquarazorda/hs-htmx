{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Folders where

import           Components.Content.Header (contentHeader)
import           Components.Table.Simple   (TableHeader (TableHeader),
                                            simpleTable)
import           Data.Discogs.Folders      (DcFolder (count),
                                            DcFolderRes (DcFolderRes, folders),
                                            foldersPath, name)
import           Data.Foldable             (Foldable (foldl'))
import           Http                      (getDcResponse)
import           Lucid                     (Html, ToHtml (toHtml), class_, div_,
                                            td_, tr_)
import           Router                    (GETRoute, PageRoute, getRoute)
import           State                     (AppM)

type FoldersRouter = PageRoute "folders"

tableHeaders :: [TableHeader]
tableHeaders = [TableHeader "Name" "", TableHeader "Count" ""]

content :: AppM (Html ())
content = do
  (mFolders :: Maybe DcFolderRes) <- getDcResponse foldersPath
  pure $ do
    contentHeader "Folders" Nothing
    div_ [class_ "w-full overflow-auto"] $ do
      simpleTable tableHeaders $
        case mFolders of
          Just (DcFolderRes { folders = f }) -> foldl' (<>) "" (drawItem f)
          _                                  -> tr_ ""
          where
            drawItem = fmap (\f' -> tr_ $ do
              td_ [class_ "p-2 align-middle"] (toHtml $ name f')
              td_ [class_ "p-2 align-middle"] (toHtml $ show $ count f')
              )

foldersRouter :: GETRoute
foldersRouter = getRoute "/folders" content
