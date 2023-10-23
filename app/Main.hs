{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Exception          (bracket, finally)
import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import           Data.String                (IsString (fromString))
import           Data.Text                  (pack)
import           Database.MySQL.Base        (ConnectInfo (ciPort), ciDatabase,
                                             ciHost, ciPassword, ciUser, close,
                                             connect, defaultConnectInfo)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setHost, setPort)
import           Routes.Categories          (CategoriesRouter, categoriesRouter)
import           Routes.Folders             (FoldersRouter, foldersRouter)
import           Routes.Home                (HomeRouter, homeRouter)
import           Routes.Products            (ProductsRouter, productsRouter)
import           Servant                    (Application, Handler,
                                             HasServer (ServerT), Proxy (..),
                                             Raw, serve,
                                             serveDirectoryFileServer,
                                             type (:<|>) (..), type (:>))
import           Servant.Server             (hoistServer)
import           State                      (AppM,
                                             DbEnv (dbName, dbPass, dbUrl, dbUsername),
                                             State (State), parseWpDbEnv,
                                             parseWpEnv)
import           System.Environment         (lookupEnv)

type API =
  "public" :> Raw
    :<|> HomeRouter
    :<|> ProductsRouter
    :<|> CategoriesRouter
    :<|> FoldersRouter

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
    :<|> categoriesRouter
    :<|> foldersRouter

main :: IO ()
main = do
  wpEnv <- parseWpEnv
  dbEnv <- parseWpDbEnv
  mHostIp <- lookupEnv "HOST_IP"
  mDcToken <- lookupEnv "DC_TOKEN"
  mPort <- lookupEnv "PORT"
  case (wpEnv, dbEnv, mHostIp, mDcToken, mPort) of
    (Just wp, Just db, Just hostIp, Just dcToken, Just port) -> do
      let connInfo = defaultConnectInfo
            { ciUser = dbUsername db
            , ciPassword = dbPass db
            , ciHost = dbUrl db
            , ciDatabase = dbName db
            , ciPort = 3306
            }
      let servantSettings = setPort (read port) $ setHost (fromString hostIp) defaultSettings
      putStrLn $ "Running on http://" <> hostIp <> ":" <> port
      runSettings servantSettings $ app (State wp (pack dcToken))
      -- bracket (connect connInfo) -- Acquire the connection
      --         close  -- Release the connection
      --         (\conn -> do
      --           let servantSettings = setPort (read port) $ setHost (fromString hostIp) defaultSettings
      --           putStrLn $ "Running on http://" <> hostIp <> ":" <> port
      --           runSettings servantSettings $ app (State wp conn (pack dcToken))
      --         ) `finally` putStrLn "Shutting down..."
    _ -> putStrLn "Failed to parse .env file."
