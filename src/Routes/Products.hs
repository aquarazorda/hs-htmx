{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Products (productsRouter, ProductsRouter) where

import           Components.Content.Header (contentHeader)
import           Components.Table.Simple   (TableHeader (TableHeader),
                                            simpleTable)
import           Data.Aeson                (decode)
import           Data.Text                 (pack)
import qualified Data.WC.Category          as Cat
import           Data.WC.Product           (WpPost (..), categories, name)
import           Http                      (getWpResponse)
import           Lucid                     (Html, ToHtml (toHtml), class_, div_,
                                            span_, td_, tr_)
import           Lucid.Htmx                (hxGet_)
import           Prelude                   hiding (id)
import           Router                    (GETRoute, RouteResponse, getRoute)
import           Servant                   (Get, (:>))
import           Servant.HTML.Lucid        (HTML)
import           Servant.Htmx              (HXRequest)
import           Shadcn.Button             (cnBtn)
import           State                     (AppM)

type ProductsRouter =
  "products" :> HXRequest :> Get '[HTML] RouteResponse

wcGetPosts :: AppM (Maybe [WpPost])
wcGetPosts = do
  res <- getWpResponse "/products"
  pure $ decode res

content :: AppM (Html ())
content = do
  maybe contentEmpty withPosts <$> wcGetPosts
  where
    wrapper :: Html () -> Html ()
    wrapper h = contentHeader "Products" Nothing <> div_ [class_ "w-full overflow-auto"] h
    contentEmpty :: Html ()
    contentEmpty = wrapper "No posts found."
    withPosts :: [WpPost] -> Html ()
    withPosts d =
      wrapper $ simpleTable tableHeaders postList
      where
        postList = foldl1 (<>) $ fmap postItem d
        tableHeaders = [TableHeader "Title" "", TableHeader "Category" "", TableHeader "Quantity" "", TableHeader "Price" "text-right", TableHeader "" ""]

postItem :: WpPost -> Html ()
postItem (WpPost {id = cId, name = cName, categories = cCategories, price = cPrice, stock_quantity = cQuantity}) =
  tr_ [class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted cursor-pointer", hxGet_ $ "/product/" <> (pack . show $ cId)] $
    td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
      <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (foldl1 (<>) $ fmap categoryItem cCategories)
      <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml (show cQuantity))
      <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0 text-right"] (toHtml cPrice)
      <> td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (cnBtn "Edit")
  where
    categoryItem :: Cat.WpCategory -> Html ()
    categoryItem (Cat.WpCategory {Cat.name = cat}) = span_ [class_ "px-2 py-1 bg-red-200 text-red-800 rounded-md mr-1"] $ toHtml cat

productsRouter :: GETRoute
productsRouter = getRoute "/products" content
