{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Categories (categoriesRouter, CategoriesRouter) where

import           Components.Content.Header  (contentHeader)
import           Components.Table.Simple    (TableHeader (TableHeader),
                                             simpleTable)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ask)
import           Lucid                      (Html, ToHtml (toHtml), class_,
                                             div_, td_, tr_)
import           Opaleye                    (runSelect)
import           Postgres.Category          (Category,
                                             Category' (Category, categoryName, categorySlug),
                                             categorySelect)
import           Router                     (GETRoute, RouteResponse, getRoute)
import           Servant                    (Get, (:>))
import           Servant.HTML.Lucid         (HTML)
import           Servant.Htmx               (HXRequest)
import           State                      (AppM, State (db))

type CategoriesRouter = "categories" :> HXRequest :> Get '[HTML] RouteResponse

categoryItem :: Category -> Html ()
categoryItem (Category{categoryName = cName, categorySlug = cSlug}) =
  td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
    <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cSlug)

content :: AppM (Html ())
content = do
  dbconn <- db <$> ask
  categories :: [Category] <- liftIO $ runSelect dbconn categorySelect
  pure
    $ contentHeader "Categories" Nothing
    <> div_
      [class_ "w-full overflow-auto"]
      ( simpleTable
          tableHeaders
          ( tr_ [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"]
              $ foldl1 (<>) (fmap categoryItem categories)
          )
      )
 where
  tableHeaders = [TableHeader "Title" "", TableHeader "Slug" ""]

categoriesRouter :: GETRoute
categoriesRouter = getRoute "/categories" content
