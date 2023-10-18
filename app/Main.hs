{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Configuration.Dotenv             (defaultConfig, loadFile)
import           Configuration.Dotenv.Environment (lookupEnv)
import           Control.Monad.Trans.Reader       (ReaderT (runReaderT))
import           Data.String                      (IsString (fromString))
import           Data.Text                        (pack)
import           Database.PostgreSQL.Simple       (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                                   defaultConnectInfo,
                                                   withConnect)
import           Network.Wai.Handler.Warp         (defaultSettings, runSettings,
                                                   setHost, setPort)
import           Routes.Folders                   (FoldersRouter, foldersRouter)
import           Routes.Home                      (HomeRouter, homeRouter)
import           Routes.Products                  (ProductsRouter,
                                                   productsRouter)
import           Servant                          (Application, Handler,
                                                   HasServer (ServerT),
                                                   Proxy (..), Raw, serve,
                                                   serveDirectoryFileServer,
                                                   type (:<|>) (..), type (:>))
import           Servant.Server                   (hoistServer)
import           State                            (AppM,
                                                   DbEnv (dbName, dbPass, dbUrl, dbUsername),
                                                   State (State), parseDbEnv,
                                                   parseWpEnv)

type API =
  "public" :> Raw
    :<|> HomeRouter
    :<|> ProductsRouter
    -- :<|> CategoriesRouter
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
    -- :<|> categoriesRouter
    :<|> foldersRouter

main :: IO ()
main = do
  loadFile defaultConfig
  wpEnv <- parseWpEnv
  dbEnv <- parseDbEnv
  mHostIp <- lookupEnv "HOST_IP"
  mDcToken <- lookupEnv "DC_TOKEN"
  case (wpEnv, dbEnv, mHostIp, mDcToken) of
    (Just wp, Just db, Just hostIp, Just dcToken) -> do
      -- let connectionInfo = defaultConnectInfo {
      --   connectHost = dbUrl db,
      --   connectDatabase = dbName db,
      --   connectUser = dbUsername db,
      --   connectPassword = dbPass db
      -- }
      let servantSettings = setPort 8080 $ setHost (fromString hostIp) defaultSettings
      -- withConnect connectionInfo $ \dbconn -> do
      putStrLn $ "Running on http://" <> hostIp <> ":8080"
      runSettings servantSettings $ app (State wp (pack dcToken))
    _ -> putStrLn "Failed to parse .env file."
