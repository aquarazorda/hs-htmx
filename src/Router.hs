{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Router (routePage, PageResponse, PageRoute, getRoute, GETRoute) where

import           Components.Navbar     (navBar)
import           Components.Spinner    (spinner)
import           Data.Foldable         (foldl')
import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Lucid                 (Html, HtmlT, body_, class_,
                                        crossorigin_, doctype_, head_, href_,
                                        id_, link_, main_, rel_, script_, src_)
import           Lucid.Htmx            (useHtmx)
import           Servant               (Get, Header, Headers, addHeader,
                                        noHeader, (:>))
import           Servant.HTML.Lucid    (HTML)
import           Servant.Htmx          (HXPush, HXRequest)
import           State                 (AppM)
import           Web.Cookie            (CookiesText, parseCookiesText)

type PageRoute a = a :> Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse
type PageResponse = Headers '[HXPush, Header "Vary" String, Header "Cache-Control" String] (Html ())

routePage :: CookiesText -> Text -> HtmlT Identity () -> Html ()
routePage cookies path content = do
  doctype_
  head_ $ do
      useHtmx
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.11"] ("" :: Html ())
      link_ [rel_ "stylesheet", href_ "/public/styles.css"]
      link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
      link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
      link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"]
  body_ [
    class_ $ "font-sans flex" <> if isDarkMode then " dark" else "", id_ "body"] $ do
    navBar path
    spinner "router-loader" "htmx-request:flex hidden"
    main_ [class_ "flex-grow p-6 htmx-request:hidden", id_ "router-outlet"] content
  where
    isDarkMode = foldl' (\acc (name, value) -> acc || (name == "darkMode" && value == "true")) False cookies

type GETRoute = Maybe Text -> Maybe Text -> AppM PageResponse

getRoute :: Text -> AppM (Html ()) -> GETRoute
getRoute path content mCookies hx = case hx of
  Nothing -> (noHeader . commonHeaders) . routePage cookies path <$> content
  Just _  -> addHeader path . commonHeaders <$> content
  where
    commonHeaders = addHeader "HX-Request" . addHeader "max-age=180"
    cookies = case mCookies of
      Nothing -> []
      Just c  -> parseCookiesText $ encodeUtf8 c
