{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import           Data.String                (IsString (fromString))
import           Data.Text                  (pack)
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
import           State                      (AppM, State (State), parseDbEnv,
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
  dbEnv <- parseDbEnv
  mHostIp <- lookupEnv "HOST_IP"
  mDcToken <- lookupEnv "DC_TOKEN"
  mPort <- lookupEnv "PORT"
  case (wpEnv, dbEnv, mHostIp, mDcToken, mPort) of
    (Just wp, Just db, Just hostIp, Just dcToken, Just port) -> do
      -- let connectionInfo = defaultConnectInfo {
      --   connectHost = dbUrl db,
      --   connectDatabase = dbName db,
      --   connectUser = dbUsername db,
      --   connectPassword = dbPass db
      -- }
      let servantSettings = setPort (read port) $ setHost (fromString hostIp) defaultSettings
      -- withConnect connectionInfo $ \dbconn -> do
      putStrLn $ "Running on http://" <> hostIp <> ":" <> port
      runSettings servantSettings $ app (State wp (pack dcToken))
    _ -> putStrLn "Failed to parse .env file."
