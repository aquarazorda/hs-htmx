{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant ( Proxy(..), serve, Get, Server, Application )
import Lucid (Html, p_, div_, ToHtml (toHtml), head_)
import Servant.HTML.Lucid (HTML)
import Lucid.Htmx (useHtmx)

type HelloWorldAPI = Get '[HTML] (Html ())

server :: Server HelloWorldAPI
server = return helloWorldHtml

helloWorldHtml :: Html ()
helloWorldHtml = head_ useHtmx <> div_ (p_ $ toHtml "Hello World!") 

helloWorldAPI :: Proxy HelloWorldAPI
helloWorldAPI = Proxy

app :: Application
app = serve helloWorldAPI server

main :: IO ()
main = run 8080 app
