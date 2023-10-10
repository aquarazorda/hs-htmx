{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Home (homeRouter, HomeRouter) where

import           Components.Content.Header (contentHeader)
import           Lucid                     (Html)
import           Router                    (GETRoute, RouteResponse, getRoute)
import           Servant                   (Get, (:>))
import           Servant.HTML.Lucid        (HTML)
import           Servant.Htmx              (HXRequest)

type HomeRouter = HXRequest :> Get '[HTML] RouteResponse

content :: Html ()
content = contentHeader "Home" Nothing <> "Welcome to Morevi.ge dashboard."

homeRouter :: GETRoute
homeRouter = getRoute "/" $ pure content
