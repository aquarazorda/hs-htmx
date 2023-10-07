{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Configuration.Dotenv.Environment (lookupEnv)
import Network.Wai.Handler.Warp (run)
import Routes.Home (HomeRouter, homeRouter)
import Routes.Posts (PostsRouter, postsRouter)
import Servant
  ( Application,
    Proxy (..),
    Raw,
    Server,
    serve,
    serveDirectoryFileServer,
    type (:<|>) (..),
    type (:>),
  )

type API =
  "public" :> Raw
    :<|> HomeRouter
    :<|> PostsRouter

data Env = Env
  { wpUrl :: String,
    wpKey :: String,
    wpSecret :: String
  } deriving (Show)

parseEnv :: IO (Maybe Env)
parseEnv = do
  loadFile defaultConfig
  mUrl <- lookupEnv "wpUrl"
  mKey <- lookupEnv "wpKey"
  mSecret <- lookupEnv "wpSecret"
  case (mUrl, mKey, mSecret) of
    (Just url, Just key, Just secret) -> pure $ Just $ Env url key secret
    _ -> pure Nothing

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
  env <- parseEnv
  print env
  putStrLn "Running on http://localhost:8080"
  run 8080 app
