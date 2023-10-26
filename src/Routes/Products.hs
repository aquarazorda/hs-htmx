{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Products (productsApi, ProductsApi) where

import Components.Content.Header (contentHeader)
import Components.Product.SaveForm (productSaveForm)
import Components.Shadcn.Button (cnBtn)
import Components.Table.Simple
  ( TableHeader (TableHeader)
  , simpleTable
  )
import Data.Discogs.Release (DcReleaseForm, generateWpPostData)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.WC.Category as Cat
import Data.WC.Product (WpPost (..), WpProductResponse)
import Http (getWpResponse, postWp)
import Lucid
  ( Html
  , ToHtml (toHtml)
  , class_
  , div_
  , span_
  , td_
  , tr_
  )
import Lucid.Htmx (hxGet_)
import Router.Helpers (GenericResponse, getRoute, toUrlPiece_)
import Routes.AddProduct (addProductList)
import Routes.Folders (folderContent)
import Servant
  ( err422
  , fieldLink
  , throwError
  )
import Servant.Server.Generic (AsServerT)
import State (AppM)
import Types.Api (ProductsApi (ProductsApi, getProducts, getRelease, postRelease))
import Prelude hiding (concat, id)

productsApi :: ProductsApi (AsServerT AppM)
productsApi =
  ProductsApi
    { getProducts = getRoute "/products" content
    , getRelease = releaseContent
    , postRelease = releasePost
    }

type ReleasePost = DcReleaseForm -> Maybe Int -> Maybe Int -> Maybe Text -> GenericResponse

releasePost :: ReleasePost
releasePost formData folderId mPage query cookies hx = do
  (res :: Maybe WpProductResponse) <- postWp "/products" $ generateWpPostData formData
  case res of
    Nothing -> throwError err422
    Just _ -> do
      case folderId of
        Just fId -> folderContent fId mPage cookies hx
        Nothing -> addProductList query Nothing cookies hx

type ReleaseContent = Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Int -> GenericResponse

releaseContent :: ReleaseContent
releaseContent releaseId mPrice mCondition mFolderId mSearch mPage =
  getRoute
    (toUrlPiece_ (fieldLink getRelease releaseId mPrice mCondition mFolderId mSearch mPage))
    (productSaveForm price page condition mFolderId mSearch releaseId)
 where
  price = fromMaybe "0.00" mPrice
  condition = fromMaybe "VG+" mCondition
  page = fromMaybe 1 mPage

content :: AppM (Html ())
content = do
  maybe contentEmpty withPosts <$> getWpResponse "/products"
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
postItem (WpPost {wpPostId = cId, wpPostName = cName, wpPostCategories = cCategories, wpPostPrice = cPrice, wpPostStockQuantity = cQuantity}) =
  tr_
    [ class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted cursor-pointer"
    , hxGet_ $ "/product/" <> (pack . show $ cId)
    ]
    $ do
      td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
      td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (foldl1 (<>) $ fmap categoryItem cCategories)
      td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml (show cQuantity))
      td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0 text-right"] (toHtml cPrice)
      td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (cnBtn "Edit")
 where
  categoryItem :: Cat.WpCategory -> Html ()
  categoryItem (Cat.WpCategory {Cat.name = cat}) = span_ [class_ "px-2 py-1 bg-red-200 text-red-800 rounded-md mr-1"] $ toHtml cat
