module State where

import Control.Monad.Trans.Reader (ReaderT)
import Servant (Handler)
import Data.Text (Text, pack)
import Configuration.Dotenv.Environment (lookupEnv)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection (Connection)

type AppM = ReaderT State Handler

data WpEnv = WpEnv {
  wpUrl :: Text,
  wpToken :: Text
} deriving (Show)

data DbEnv = DbEnv {
  dbUrl :: ByteString,
  dbUsername :: ByteString,
  dbPass :: ByteString,
  dbName :: ByteString
} deriving (Show)

data State = State
  { wp :: WpEnv
  , db :: Connection
  }

packStr :: String -> ByteString
packStr = encodeUtf8 . pack

parseWpEnv :: IO (Maybe WpEnv)
parseWpEnv = do
  mUrl <- lookupEnv "wpUrl"
  mToken <- lookupEnv "wpToken"
  case (mUrl, mToken) of
    (Just url, Just token) -> pure $ Just $ WpEnv (pack url) (pack token)
    _ -> pure Nothing

parseDbEnv :: IO (Maybe DbEnv)
parseDbEnv = do
    mUrl <- lookupEnv "DB_URL"
    mUsername <- lookupEnv "DB_USERNAME"
    mPass <- lookupEnv "DB_PASSWORD"
    mName <- lookupEnv "DB_NAME"
    case (mUrl, mUsername, mPass, mName) of
        (Just url, Just username, Just pass, Just name) -> 
            pure $ Just $ DbEnv (packStr url) (packStr username) (packStr pass) (packStr name)
        _ -> pure Nothing