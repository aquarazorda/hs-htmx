{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Home (homeRouter, HomeRouter) where

import Data.Functor.Identity (Identity)
import Lucid (HtmlT)
import Router (RouteResponse, getRoute, GETRoute)
import Servant ((:>), Get)
import Servant.Htmx (HXRequest)
import Servant.HTML.Lucid (HTML)

type HomeRouter = HXRequest :> Get '[HTML] RouteResponse

content :: HtmlT Identity ()
content = "Welcome to Morevi.ge dashboard."

homeRouter :: GETRoute
homeRouter = getRoute $ pure content