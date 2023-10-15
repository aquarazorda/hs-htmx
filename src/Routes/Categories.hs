{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Routes.Categories (categoriesRouter, CategoriesRouter) where

import           Components.Content.Header  (contentHeader)
import           Components.Shadcn.Button   (cnBtn)
import           Components.Shadcn.Input    (cnInput)
import           Components.Shadcn.Table    (tableCell_, tableRow_)
import           Components.Table.Simple    (TableHeader (TableHeader),
                                             simpleTable)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ask)
import           Data.Text                  (Text)
import           Lucid                      (Html, ToHtml (toHtml), class_,
                                             div_, form_, id_, name_, type_)
import           Lucid.Htmx                 (hxPost_, hxSwap_, hxTarget_)
import           Opaleye                    (Insert (Insert), rReturning,
                                             runInsert, runSelect, sqlString)
import           Postgres.Category          (Category,
                                             Category' (Category, categoryName, categorySlug),
                                             CategoryForm (CategoryForm),
                                             categoryExistsSelect,
                                             categorySelect, categoryTable)
import           Router                     (GETRoute, PageRoute, getRoute)
import           Servant                    (FormUrlEncoded, Header, Headers,
                                             Post, ReqBody, addHeader, noHeader,
                                             type (:<|>) (..), (:>))
import           Servant.HTML.Lucid         (HTML)
import           Servant.Htmx               (HXRetarget)
import           State                      (AppM, State (db))

type HXReswap = Header "HX-Reswap" Text

type AddCategoryHandlerResponse = Headers '[HXRetarget, HXReswap] (Html ())

type CategoriesRouter = PageRoute "categories"
  :<|>  "categories" :> "add" :> ReqBody '[FormUrlEncoded] CategoryForm :> Post '[HTML] AddCategoryHandlerResponse

categoriesRouter :: GETRoute :<|> (CategoryForm -> AppM AddCategoryHandlerResponse)
categoriesRouter = getRoute "/categories" content :<|> addCategory

formId :: Text
formId = "add_category_form"

categoryItem :: Category -> Html ()
categoryItem (Category{categoryName = cName, categorySlug = cSlug}) = tableRow_ $ do
  tableCell_ [class_ "p-2 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cName)
  tableCell_ [class_ "p-2 align-middle [&amp;:has([role=checkbox])]:pr-0"] (toHtml cSlug)
  tableCell_ [class_ "p-2 align-middle [&amp;:has([role=checkbox])]:pr-0"] ""

addItemForm :: Html ()
addItemForm = tableRow_ [id_ "add-item"] $ do
  tableCell_ [class_ "p-2 align-middle"] $ cnInput [type_ "text", name_ "name", form_ formId]
  tableCell_ [class_ "p-2 align-middle"] $ cnInput [type_ "text", name_ "slug", form_ formId]
  tableCell_ [class_ "p-2 align-middle"] $ cnBtn [type_ "submit", form_ formId] "Save"

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
                [] -> tableRow_ $ tableCell_ [class_ "p-2 align-middle"] "Categories are empty"
                _  -> foldl1 (<>) (fmap categoryItem categories)
          addItemForm
        div_ [id_ "error-message", class_ "p-2 text-red-500"] ""

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