{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Types.Api where

import Data.Discogs.Release (DcReleaseForm)
import Data.Text (Text)
import GHC.Generics (Generic)
import Router.Helpers (PageResponse, PageRoute)
import Servant
    ( Capture
    , FormUrlEncoded
    , GenericMode (type (:-))
    , Get
    , Header
    , Post
    , QueryParam
    , ReqBody
    , type (:>)
    )
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXRequest)

data FoldersApi mode = FoldersApi
    { getFolders :: mode :- "folders" :> PageRoute
    , getFolder
        :: mode
            :- "folders"
                :> Capture "folderId" Int
                :> QueryParam "page" Int
                :> PageRoute
    }
    deriving (Generic)

newtype AddProductApi mode = AddProductApi
    { addProduct :: mode :- "add-product" :> QueryParam "search" Text :> Header "Partial" Bool :> PageRoute
    }
    deriving (Generic)

data ProductsApi mode = ProductsApi
    { getProducts :: mode :- "products" :> Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse
    , getRelease
        :: mode
            :- "release"
                :> Capture "releaseId" Int
                :> QueryParam "price" Text
                :> QueryParam "condition" Text
                :> QueryParam "folderId" Int
                :> QueryParam "search" Text
                :> QueryParam "page" Int
                :> PageRoute
    , postRelease
        :: mode
            :- "release"
                :> ReqBody '[FormUrlEncoded] DcReleaseForm
                :> QueryParam "folderId" Int
                :> QueryParam "page" Int
                :> QueryParam "search" Text
                :> HXRequest
                :> Header "Cookie" Text
                :> Post '[HTML] PageResponse
    }
    deriving (Generic)