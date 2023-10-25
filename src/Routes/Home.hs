{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Home (homeApi, HomeApi) where

import           Components.Content.Header (contentHeader)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Lucid                     (Html)
import           Router                    (PageResponse, getRoute)
import           Servant                   (Get, Header, (:-), (:>))
import           Servant.HTML.Lucid        (HTML)
import           Servant.Htmx              (HXRequest)
import           Servant.Server.Generic    (AsServerT)
import           State                     (AppM)

newtype HomeApi mode = HomeApi
  { getHome :: mode :- Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse
  } deriving (Generic)

homeApi :: HomeApi (AsServerT AppM)
homeApi = HomeApi
  { getHome = getRoute "/" $ pure content
  }


content :: Html ()
content = contentHeader "Home" Nothing <> "Welcome to Morevi.ge dashboard."
