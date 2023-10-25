{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.AddProduct where

import           GHC.Generics           (Generic)
import           Lucid                  (Html)
import           Servant
import           Servant.HTML.Lucid     (HTML)
import           Servant.Server.Generic (AsServerT)
import           State                  (AppM)

newtype AddProductApi mode = AddProductApi
  { addProduct :: mode :- "add-product" :> Get '[HTML] (Html ())
  } deriving (Generic)

addProductApi :: AddProductApi (AsServerT AppM)
addProductApi = AddProductApi
  { addProduct = addProductList
  }

addProductList :: AppM (Html ())
addProductList = pure "Add Product List"
