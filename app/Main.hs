{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant
    ( serve,
      serveDirectoryFileServer,
      Proxy(..),
      type (:<|>)(..),
      Raw,
      type (:>),
      Server,
      Application )
import Routes.Posts (postsRouter, PostsRouter)
import Routes.Home (homeRouter, HomeRouter)

type API =
  "public" :> Raw
    :<|> HomeRouter
    :<|> PostsRouter

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server =
  serveDirectoryFileServer "public"
    :<|> homeRouter
    :<|> postsRouter

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080"
  run 8080 app
