{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Postgres.Category where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Field,
                                             InferrableTableField (tableField),
                                             Select, SqlInt4, SqlText, Table,
                                             selectTable, table)

data Category' a b c = Category
  { categoryId   :: a
  , categoryName :: b
  , categorySlug :: c
  } deriving (Show)
type Category = Category' Int String String
type CategoryField = Category' (Field SqlInt4) (Field SqlText) (Field SqlText)

$(makeAdaptorAndInstance "pCategory" ''Category')

categoryTable :: Table CategoryField CategoryField
categoryTable = table "category" $ pCategory Category
  {
    categoryId = tableField "id",
    categoryName = tableField "name",
    categorySlug = tableField "slug"
  }

categorySelect :: Select CategoryField
categorySelect = selectTable categoryTable
