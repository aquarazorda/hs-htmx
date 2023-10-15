{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Home (homeRouter, HomeRouter) where

import           Components.Content.Header (contentHeader)
import           Data.Text                 (Text)
import           Lucid                     (Html)
import           Router                    (GETRoute, RouteResponse, getRoute)
import           Servant                   (Get, Header, (:>))
import           Servant.HTML.Lucid        (HTML)
import           Servant.Htmx              (HXRequest)

type HomeRouter = Header "Cookie" Text :> HXRequest :> Get '[HTML] RouteResponse

content :: Html ()
content = contentHeader "Home" Nothing <> "Welcome to Morevi.ge dashboard."

homeRouter :: GETRoute
homeRouter = getRoute "/" $ pure content
