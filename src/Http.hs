{-# LANGUAGE OverloadedStrings #-}

module Http (getManager, getWpResponse, getDcResponse, postWp) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ask)
import           Data.Aeson                 (FromJSON, ToJSON, decode)
import           Data.Function              ((&))
import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (encodeUtf8)
import           Network.HTTP.Client        (Manager, httpLbs, newManager,
                                             parseRequest_)
import           Network.HTTP.Conduit       (tlsManagerSettings)
import           Network.HTTP.Simple        (getResponseBody,
                                             setRequestBodyJSON,
                                             setRequestHeaders,
                                             setRequestMethod)
import           State                      (AppM, State (dcToken, wp),
                                             WpEnv (wpToken, wpUrl))

getManager :: IO Manager
getManager = newManager tlsManagerSettings

getHeaders preToken token = [("Authorization",  encodeUtf8 $ preToken <> token), ("User-Agent", "Morevi/0.1")]

getResponse :: FromJSON a => Text -> Text -> Text -> AppM (Maybe a)
getResponse preToken token path = do
  manager <- liftIO getManager
  let reqHeaders = getHeaders preToken token
  let req = setRequestHeaders reqHeaders $ parseRequest_ $ unpack path
  res <- liftIO $ httpLbs req manager
  pure $ decode (getResponseBody res)

postResponse :: (ToJSON a, FromJSON b)=> Text -> Text -> Text -> a -> AppM (Maybe b)
postResponse preToken token path json = do
  manager <- liftIO getManager
  let reqHeaders = getHeaders preToken token
  let req = parseRequest_ (unpack path) & setRequestHeaders reqHeaders & setRequestMethod "POST" & setRequestBodyJSON json
  res <- liftIO $ httpLbs req manager
  pure $ decode (getResponseBody res)

getDcResponse :: FromJSON a => Text -> AppM (Maybe a)
getDcResponse path = do
  env <- ask
  getResponse "Discogs token=" (dcToken env) ("https://api.discogs.com" <> path)

getWpResponse :: FromJSON a => Text -> AppM (Maybe a)
getWpResponse path = do
  env <- wp <$> ask
  getResponse "Basic " (wpToken env) (wpUrl env <> path)

postWp :: (ToJSON a, FromJSON b) => Text -> a -> AppM (Maybe b)
postWp path json = do
  env <- wp <$> ask
  postResponse "Basic " (wpToken env) (wpUrl env <> path) json
