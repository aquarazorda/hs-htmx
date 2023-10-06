{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Lucid (Html, class_, div_, head_, p_, rel_, link_, href_)
import Lucid.Htmx (useHtmx)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)

type API =
  "public" :> Raw
    :<|> "hello" :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app

--------------------

server :: Server API
server =
  serveDirectoryFileServer "public"
    :<|> return helloWorldHtml

helloWorldHtml :: Html ()
helloWorldHtml = head_ (useHtmx <> link_ [href_ "/public/styles.css", rel_ "stylesheet"]) <> div_ [class_ "bg-black"] (p_ "Hello World!")
