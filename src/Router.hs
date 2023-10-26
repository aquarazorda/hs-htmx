{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Router where

import GHC.Generics (Generic)
import Routes.AddProduct (addProductApi)
import Routes.Categories (CategoriesApi, categoriesApi)
import Routes.Folders (foldersApi)
import Routes.Home (HomeApi, homeApi)
import Routes.Products (ProductsApi, productsApi)
import Servant.Server.Generic (AsServerT)
import Types.Api (AddProductApi, FoldersApi)

import Servant
  ( NamedRoutes
  , Raw
  , ToServantApi
  , serveDirectoryFileServer
  , (:-)
  , (:>)
  )
import State (AppM)

type API = ToServantApi RootApi

data RootApi mode = RootApi
  { public :: mode :- "public" :> Raw
  , home :: mode :- NamedRoutes HomeApi
  , products :: mode :- NamedRoutes ProductsApi
  , folders :: mode :- NamedRoutes FoldersApi
  , categories :: mode :- NamedRoutes CategoriesApi
  , addProducts :: mode :- NamedRoutes AddProductApi
  }
  deriving (Generic)

apiRoutes :: RootApi (AsServerT AppM)
apiRoutes =
  RootApi
    { public = serveDirectoryFileServer "public"
    , home = homeApi
    , products = productsApi
    , folders = foldersApi
    , categories = categoriesApi
    , addProducts = addProductApi
    }
