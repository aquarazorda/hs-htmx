{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.AddProduct where

import Components.Content.Header (contentHeader)
import Components.Shadcn.Input (cnInput)
import Components.Spinner (spinner)
import Data.Discogs.Search (DcSearchRelease (..), DcSearchRes (..))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text, empty)
import Htmx (navChangeAttrs)
import Http (getDcResponse)
import Lucid (Html, ToHtml (toHtml), autofocus_, class_, div_, h3_, id_, img_, name_, p_, placeholder_, src_, type_, value_)
import Lucid.Htmx (hxGet_, hxHeaders_, hxIndicator_, hxSwap_, hxTarget_, hxTrigger_)
import Router.Helpers (GenericResponse, getRoute, toUrlPiece_)
import Servant
  ( fieldLink
  )
import Servant.Server.Generic (AsServerT)
import State (AppM)
import Types.Api (AddProductApi (AddProductApi, addProduct), ProductsApi (getRelease))

addProductApi :: AddProductApi (AsServerT AppM)
addProductApi =
  AddProductApi
    { addProduct = addProductList
    }

productItemList :: Maybe Text -> AppM (Html ())
productItemList q = case q of
  Nothing -> pure ""
  Just query -> if query == empty then pure "" else withQuery query
 where
  withQuery query = do
    (res :: Maybe DcSearchRes) <- getDcResponse $ "/database/search?type=release&per_page=50&query=" <> query
    case res of
      Nothing -> pure "Can't find any product"
      Just f -> pure $ foldl' (<>) "" (fmap drawItem (dcSearchResults f))
  drawItem :: DcSearchRelease -> Html ()
  drawItem i = div_ [class_ "space-y-3 w-[150px]"] $ do
    img_
      ( [ src_ $ dcSearchRelThumb i
        , class_ "w-36 h-36 object-cover transition-all hover:scale-105 aspect-square cursor-pointer"
        ]
          <> navChangeAttrs (toUrlPiece_ $ fieldLink getRelease (dcSearchRelId i) Nothing Nothing Nothing q Nothing)
      )
    div_ [class_ "space-y-1 text-sm"] $ do
      h3_ [class_ "font-medium leading-none text-ellipsis whitespace-nowrap overflow-hidden"] ((toHtml . dcSearchRelTitle) i)
      p_ [class_ "text-xs text-muted-foreground"] (toHtml $ dcSearchRelCatno i <> " (" <> dcSearchRelYear i <> ")")

addProductList :: Maybe Text -> Maybe Bool -> GenericResponse
addProductList q partial = getRoute (toUrlPiece_ $ fieldLink addProduct q) content
 where
  isPartial = fromMaybe False partial
  content = do
    items <- productItemList q
    ( if isPartial
        then pure items
        else pure $ do
          contentHeader "Add product" Nothing
          div_ [class_ "gap-6 flex flex-col"] $ do
            cnInput
              [ class_ "w-full h-12"
              , placeholder_ "Start typing to search a record..."
              , type_ "search"
              , name_ "search"
              , value_ $ fromMaybe "" q
              , autofocus_
              , hxGet_ "/add-product"
              , hxTrigger_ "keyup changed delay:400ms, search"
              , hxTarget_ "#search-content"
              , hxSwap_ "innerHTML"
              , hxIndicator_ "#search-loader"
              , hxHeaders_ "{\"Partial\": true}"
              ]
            div_ [id_ "search-content", class_ "flex gap-6 flex-wrap"] items
            spinner "search-loader" "h-full htmx-request:flex hidden"
      )