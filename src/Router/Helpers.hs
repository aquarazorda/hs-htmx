{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Router.Helpers where

import           Components.MainWrapper (routePage)
import           Data.Text              (Text)
import           Lucid                  (Html)
import           Servant
import           Servant.HTML.Lucid     (HTML)
import           Servant.Htmx           (HXPush, HXRequest)
import           State                  (AppM)
import Web.Cookie (parseCookiesText)
import Data.Text.Encoding (encodeUtf8)


type PageRoute =  Header "Cookie" Text :> HXRequest :> Get '[HTML] PageResponse
type PageResponse = Headers '[HXPush, Header "Vary" String, Header "Cache-Control" String] (Html ())

type GenericResponse = Maybe Text -> Maybe Text -> AppM PageResponse

getRoute :: Text -> AppM (Html ()) -> GenericResponse
getRoute path content mCookies hx = case hx of
  Nothing -> (noHeader . commonHeaders) . routePage cookies path <$> content
  Just _  -> addHeader path . commonHeaders <$> content
  where
    commonHeaders = addHeader "HX-Request" . addHeader "max-age=180"
    cookies = case mCookies of
      Nothing -> []
      Just c  -> parseCookiesText $ encodeUtf8 c
