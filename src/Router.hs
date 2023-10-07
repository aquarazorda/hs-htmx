{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Router (routePage, RouteResponse, getRoute, GETRoute) where

import Components.Navbar (navBar)
import Data.Functor.Identity (Identity)
import Lucid (Html, HtmlT, body_, class_, head_, href_, id_, link_, main_, rel_)
import Lucid.Htmx (useHtmx)
import Servant (Headers, Header, Header, noHeader, addHeader)
import Servant.Htmx (HXPush)
import State (AppM)
import Data.Text (Text)

type RouteResponse = Headers '[HXPush, Header "Vary" String, Header "Cache-Control" String] (Html ())

routePage :: HtmlT Identity () -> Html ()
routePage content =
  head_ (useHtmx <> link_ [href_ "/public/styles.css", rel_ "stylesheet"])
    <> body_ [class_ "flex"] navBar
    <> main_ [class_ "flex-grow p-6", id_ "router-outlet"] content
  
type GETRoute = Maybe Text -> AppM RouteResponse

getRoute :: AppM (Html ()) -> GETRoute
getRoute content hx = case hx of
  Nothing -> (noHeader . commonHeaders) . routePage <$> content
  Just _ -> addHeader "/posts" . commonHeaders <$> content
  where
    commonHeaders = addHeader "HX-Request" . addHeader "max-age=180"