{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Postgres.Category where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           GHC.Generics               (Generic)
import           Opaleye                    (Field,
                                             InferrableTableField (tableField),
                                             Select, SqlInt4, SqlText, Table,
                                             ilike, selectTable, sqlString,
                                             table, where_, (.||))
import           Web.FormUrlEncoded         (FromForm)

data Category' a b c = Category
  { categoryId   :: a
  , categoryName :: b
  , categorySlug :: c
  } deriving (Show, Generic)
type Category = Category' Int String String
type CategoryField = Category' (Maybe (Field SqlInt4)) (Field SqlText) (Field SqlText)
type CategoryField' = Category' (Field SqlInt4) (Field SqlText) (Field SqlText)

data CategoryForm = CategoryForm
  { name :: String
  , slug :: String
  } deriving (Generic)

instance FromForm CategoryForm

$(makeAdaptorAndInstance "pCategory" ''Category')

categoryTable :: Table CategoryField CategoryField'
categoryTable = table "category" $ pCategory Category
  {
    categoryId = tableField "id",
    categoryName = tableField "name",
    categorySlug = tableField "slug"
  }

categorySelect :: Select CategoryField'
categorySelect = selectTable categoryTable

categoryExistsSelect :: String -> String -> Select CategoryField'
categoryExistsSelect cName cSlug = do
    row <- categorySelect
    where_ $ ilike (categoryName row) (sqlString cName) .|| ilike (categorySlug row) (sqlString cSlug)
    pure row
