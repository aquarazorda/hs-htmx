{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Router (routePage, RouteResponse, getRoute, GETRoute) where

import           Components.Navbar     (navBar)
import           Components.Spinner    (spinner)
import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)
import           Lucid                 (Html, HtmlT, body_, class_, doctype_,
                                        head_, href_, id_, link_, main_, rel_,
                                        script_, src_)
import           Lucid.Htmx            (useHtmx)
import           Servant               (Header, Headers, addHeader, noHeader)
import           Servant.Htmx          (HXPush)
import           State                 (AppM)

type RouteResponse = Headers '[HXPush, Header "Vary" String, Header "Cache-Control" String] (Html ())

routePage :: Text -> HtmlT Identity () -> Html ()
routePage path content = do
  doctype_
  head_ $ do
      useHtmx
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.11"] ("" :: Html ())
      link_ [href_ "/public/styles.css", rel_ "stylesheet"]
  body_ [class_ "flex", id_ "body"] $ do
    navBar path
    spinner "router-loader" "htmx-request:flex hidden"
    main_ [class_ "flex-grow p-6 htmx-request:hidden", id_ "router-outlet"] content

type GETRoute = Maybe Text -> AppM RouteResponse

getRoute :: Text -> AppM (Html ()) -> GETRoute
getRoute path content hx = case hx of
  Nothing -> (noHeader . commonHeaders) . routePage path <$> content
  Just _  -> addHeader path . commonHeaders <$> content
  where
    commonHeaders = addHeader "HX-Request" . addHeader "max-age=180"
