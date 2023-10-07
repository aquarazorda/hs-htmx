{-# LANGUAGE OverloadedStrings #-}

module Http (getManager, getWpResponse) where

import Network.HTTP.Client (newManager, Manager, parseRequest_, httpLbs)
import Control.Monad.IO.Class (MonadIO(liftIO))
import State (AppM, getEnv, Env (wpToken, wpUrl))
import Network.HTTP.Simple (setRequestHeaders, getResponseBody)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy.Internal (ByteString)
import Network.HTTP.Conduit (tlsManagerSettings)

getManager :: IO Manager
getManager = newManager tlsManagerSettings

getWpResponse :: Text -> AppM ByteString
getWpResponse path = do
  env <- getEnv
  manager <- liftIO getManager
  let reqHeaders = [("Authorization",  encodeUtf8 $ "Basic " <> wpToken env)]
  let req = setRequestHeaders reqHeaders $ parseRequest_ $ unpack $ wpUrl env <> path
  res <- liftIO $ httpLbs req manager
  pure $ getResponseBody res