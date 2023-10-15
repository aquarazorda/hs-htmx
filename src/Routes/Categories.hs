{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Routes.Categories (categoriesRouter, CategoriesRouter) where

import           Components.Content.Header  (contentHeader)
import           Components.Table.Simple    (TableHeader (TableHeader),
                                             simpleTable)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ask)
import           Data.Text                  (Text)
import           Lucid                      (Attribute, Html, ToHtml (toHtml),
                                             class_, div_, form_, id_, name_,
                                             td_, tr_, type_)
import           Lucid.Htmx                 (hxPost_, hxSwap_, hxTarget_)
import           Opaleye                    (Insert (Insert), rReturning,
                                             runInsert, runSelect, sqlString)
import           Postgres.Category          (Category,
                                             Category' (Category, categoryName, categorySlug),
                                             CategoryForm (CategoryForm),
                                             categoryExistsSelect,
                                             categorySelect, categoryTable)
import           Router                     (GETRoute, RouteResponse, getRoute)
import           Servant                    (FormUrlEncoded, Get, Header,
                                             Headers, Post, ReqBody, addHeader,
                                             noHeader, type (:<|>) (..), (:>))
import           Servant.HTML.Lucid         (HTML)
import           Servant.Htmx               (HXRequest, HXRetarget)
import           Shadcn.Button              (cnBtn)
import           Shadcn.Input               (cnInput)
import           State                      (AppM, State (db))

type HXReswap = Header "HX-Reswap" Text

type AddCategoryHandlerResponse = Headers '[HXRetarget, HXReswap] (Html ())

type CategoriesRouter = "categories" :> Header "Cookie" Text :> HXRequest :> Get '[HTML] RouteResponse
  :<|>  "categories" :> "add" :> ReqBody '[FormUrlEncoded] CategoryForm :> Post '[HTML] AddCategoryHandlerResponse

formId :: Text
formId = "add_category_form"

trCls :: Attribute
trCls = class_ "border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"

categoryItem :: Category -> Html ()
categoryItem (Category{categoryName = cName, categorySlug = cSlug}) = tr_ [trCls] $ do
  td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
  td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cSlug)
  td_ [class_ "p-4 align-middle [&amp;:has([role=checkbox])]:pr-0"] ""

addItemForm :: Html ()
addItemForm = tr_ [id_ "add-item", trCls] $ do
  td_ [class_ "p-4 align-middle"] $ cnInput [type_ "text", name_ "name", form_ formId]
  td_ [class_ "p-4 align-middle"] $ cnInput [type_ "text", name_ "slug", form_ formId]
  td_ [class_ "p-4 align-middle"] $ cnBtn [type_ "submit", form_ formId] "Save"

content :: AppM (Html ())
content = do
  dbconn <- db <$> ask
  categories :: [Category] <- liftIO $ runSelect dbconn categorySelect
  pure $ do
      contentHeader "Categories" Nothing
      div_ [class_ "w-full overflow-auto"] $ do
        form_ [id_ formId, hxPost_ "/categories/add", hxTarget_ "#add-item", hxSwap_ "outerHTML"] ""
        simpleTable tableHeaders $ do
          case categories of
                [] -> tr_ [trCls] $ td_ [class_ "p-4 align-middle"] "Categories are empty"
                _  -> foldl1 (<>) (fmap categoryItem categories)
          addItemForm
        div_ [id_ "error-message", class_ "p-4 text-red-500"] ""

 where
  tableHeaders = [TableHeader "Title" "", TableHeader "Slug" "", TableHeader "" ""]

addCategory :: CategoryForm -> AppM AddCategoryHandlerResponse
addCategory (CategoryForm cName cSlug) = do
  dbconn <- db <$> ask
  (res :: [Category]) <- liftIO $ runSelect dbconn (categoryExistsSelect cName cSlug)
  case res of
    [] -> do
      (cat :: [Category]) <- liftIO $ runInsert dbconn (
        Insert categoryTable  [Category Nothing (sqlString cName) (sqlString cSlug)]  (rReturning id) Nothing
        )
      return $ case cat of
        (c:_) -> noHeader $ noHeader $ do
            categoryItem c
            addItemForm
        _ -> addHeader "#error-message" $ addHeader "innerHTML" "Something went wrong"
    _ -> return $ addHeader "#error-message" $ addHeader "innerHTML" "Category already exists"

categoriesRouter :: GETRoute :<|> (CategoryForm -> AppM AddCategoryHandlerResponse)
categoriesRouter = (getRoute "/categories" content) :<|> addCategory
