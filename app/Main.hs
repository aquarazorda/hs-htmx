{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import           Database.PostgreSQL.Simple (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                             defaultConnectInfo, withConnect)
import           Network.Wai.Handler.Warp   (run)
import           Routes.Categories          (CategoriesRouter, categoriesRouter)
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
                                             State (State), parseDbEnv,
                                             parseWpEnv)

type API =
  "public" :> Raw
    :<|> HomeRouter
    :<|> ProductsRouter
    :<|> CategoriesRouter

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

main :: IO ()
main = do
  loadFile defaultConfig
  wpEnv <- parseWpEnv
  dbEnv <- parseDbEnv
  case (wpEnv, dbEnv) of
    (Just wp, Just db) -> do
      let connectionInfo = defaultConnectInfo {
        connectHost = dbUrl db,
        connectDatabase = dbName db,
        connectUser = dbUsername db,
        connectPassword = dbPass db
      }
      withConnect connectionInfo $ \dbconn -> do
        putStrLn "Running on http://localhost:8080"
        run 8080 $ app (State wp dbconn)
    _ -> putStrLn "Failed to parse .env file."
