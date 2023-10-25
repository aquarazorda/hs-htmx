{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Routes.Categories (CategoriesApi, categoriesApi) where

import           Components.Shadcn.Label  (cnLabel)
import           Components.Shadcn.Toggle (ToggleSize (DefaultSize),
                                           ToggleVariant (Outline), cnToggle)
import           Data.List                (foldl')
import           Data.Text                (Text, pack, toLower)
import           Data.WC.Category         (WpCategory (count, name, wpCatId))
import           GHC.Generics             (Generic)
import           Http                     (getWpResponse)
import           Lucid                    (Html, ToHtml (toHtml), class_, div_,
                                           fieldset_, for_, id_, p_, span_)
import           Lucid.Base               (makeAttribute)
import           Lucid.Hyperscript        (__)
import           Servant                  (Get, Header, Headers, addHeader,
                                           (:-), (:>))
import           Servant.HTML.Lucid       (HTML)
import           Servant.Server.Generic   (AsServerT)
import           State                    (AppM)

-- type HXReswap = Header "HX-Reswap" Text

-- type AddCategoryHandlerResponse = Headers '[HXRetarget, HXReswap] (Html ())

-- type CategoriesRouter = "categories" :> PageRoute
--   :<|> "categories" :> "add" :> ReqBody '[FormUrlEncoded] CategoryForm :> Post '[HTML] AddCategoryHandlerResponse

-- categoriesRouter :: GenericResponse :<|> (CategoryForm -> AppM AddCategoryHandlerResponse)
-- categoriesRouter = getRoute "/categories" content :<|> addCategory

newtype CategoriesApi mode = CategoriesApi
  { _getCategories :: mode :- "categories" :> "wp" :> Get '[HTML] GetCategories
  } deriving (Generic)

categoriesApi :: CategoriesApi (AsServerT AppM)
categoriesApi = CategoriesApi { _getCategories = getCategories }

type GetCategories = Headers '[Header "Cache-Control" String] (Html ())

getCategories :: AppM GetCategories
getCategories = do
  (catRes :: Maybe [WpCategory]) <- getWpResponse "/products/categories?per_page=100&orderby=count&order=desc"
  pure $ case catRes of
    Just cats -> addHeader "max-age=180" $ drawCategories cats
    Nothing   -> addHeader "max-age=0" $ toHtml ("Error loading categories" :: Text)

drawCategories :: [WpCategory] -> Html ()
drawCategories categories = div_ [class_ "flex flex-col flex-1 space-y-2"] $ do
  cnLabel [for_ "release-title"] "Category"
  fieldset_ [class_ "flex gap-1 flex-wrap h-56 overflow-y-auto scrollbar-hide border p-2 rounded-md"] $ do
    foldl' (<>) "" $ fmap catItem categories
  where
    catItem :: WpCategory -> Html ()
    catItem c = cnToggle Outline DefaultSize (pack $ show $ wpCatId c) "category"
      [id_ "category-button", class_ "flex-1 whitespace-nowrap"
        , __ "get the (innerHTML of first <span/> in me) as an Int if my @data-state is on decrement it else increment it end put it into <span/> in me"
        , makeAttribute "data-value" $ toLower name'
      ]
      $ do
        div_ [class_ "flex gap-2"] $ do
          p_ $ toHtml (name c)
          span_ $ toHtml (show $ count c)
        where
          name' = pack $ name c

-- formId :: Text
-- formId = "add_category_form"

-- categoryItem :: Category -> Html ()
-- categoryItem (Category{categoryName = cName, categorySlug = cSlug}) = tableRow_ $ do
--   tableCell_ [class_ "p-2 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
--   tableCell_ [class_ "p-2 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cSlug)
--   tableCell_ [class_ "p-2 align-middle [&amp;:has([role=checkbox])]:pr-0"] ""

-- addItemForm :: Html ()
-- addItemForm = tableRow_ [id_ "add-item"] $ do
--   tableCell_ [class_ "p-2 align-middle"] $ cnInput [type_ "text", name_ "name", form_ formId]
--   tableCell_ [class_ "p-2 align-middle"] $ cnInput [type_ "text", name_ "slug", form_ formId]
--   tableCell_ [class_ "p-2 align-middle"] $ cnBtn [type_ "submit", form_ formId] "Save"

-- content :: AppM (Html ())
-- content = do
--   dbconn <- db <$> ask
--   categories :: [Category] <- liftIO $ runSelect dbconn categorySelect
--   pure $ do
--       contentHeader "Categories" Nothing
--       div_ [class_ "w-full overflow-auto"] $ do
--         form_ [id_ formId, hxPost_ "/categories/add", hxTarget_ "#add-item", hxSwap_ "outerHTML"] ""
--         simpleTable tableHeaders $ do
--           case categories of
--                 [] -> tableRow_ $ tableCell_ [class_ "p-2 align-middle"] "Categories are empty"
--                 _  -> foldl1 (<>) (fmap categoryItem categories)
--           addItemForm
--         div_ [id_ "error-message", class_ "p-2 text-red-500"] ""

--  where
--   tableHeaders = [TableHeader "Title" "", TableHeader "Slug" "", TableHeader "" ""]

-- addCategory :: CategoryForm -> AppM AddCategoryHandlerResponse
-- addCategory (CategoryForm cName cSlug) = do
--   dbconn <- db <$> ask
--   (res :: [Category]) <- liftIO $ runSelect dbconn (categoryExistsSelect cName cSlug)
--   case res of
--     [] -> do
--       (cat :: [Category]) <- liftIO $ runInsert dbconn (
--         Insert categoryTable  [Category Nothing (sqlString cName) (sqlString cSlug)]  (rReturning id) Nothing
--         )
--       return $ case cat of
--         (c:_) -> noHeader $ noHeader $ do
--             categoryItem c
--             addItemForm
--         _ -> addHeader "#error-message" $ addHeader "innerHTML" "Something went wrong"
--     _ -> return $ addHeader "#error-message" $ addHeader "innerHTML" "Category already exists"
