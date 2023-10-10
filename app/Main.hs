{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Routes.Home (HomeRouter, homeRouter)
import Routes.Products (ProductsRouter, productsRouter)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Servant
  ( Application,
    Proxy (..),
    Raw,
    Handler,
    serve,
    serveDirectoryFileServer,
    type (:<|>) (..),
    type (:>), HasServer (ServerT),
  )
import Servant.Server (hoistServer)
import State (State (State), AppM, parseWpEnv, parseDbEnv, DbEnv (dbUrl, dbUsername, dbPass, dbName))
import Configuration.Dotenv (loadFile, defaultConfig)
import Hasql.Connection (settings, acquire)

type API =
  "public" :> Raw
    :<|> HomeRouter
    :<|> ProductsRouter

api :: Proxy API
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

server :: ServerT API AppM
server =
  serveDirectoryFileServer "public"
    :<|> homeRouter
    :<|> productsRouter

main :: IO ()
main = do
  loadFile defaultConfig
  wpEnv <- parseWpEnv
  dbEnv <- parseDbEnv
  case (wpEnv, dbEnv) of
    (Just wp, Just db) -> do
      let poolSettings = settings (dbUrl db) 5432 (dbUsername db) (dbPass db) (dbName db)
      ePool <- acquire poolSettings
      case ePool of
        Left err -> putStrLn $ "Failed to connect to database: " ++ show err
        Right pool -> do
          putStrLn "Running on http://localhost:8080"
          run 8080 $ app (State wp pool)
    _ -> putStrLn "Failed to parse .env file."