module State where

import           Control.Monad.Trans.Reader (ReaderT)
import           Data.ByteString            (ByteString)
import           Data.ByteString.UTF8       (fromString)
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)
import           Database.MySQL.Base        (MySQLConn)
import           Servant                    (Handler)
import           System.Environment         (lookupEnv)

type AppM = ReaderT State Handler

data WpEnv = WpEnv {
  wpUrl   :: Text,
  wpToken :: Text
} deriving (Show)

data DbEnv = DbEnv {
  dbUrl      :: String,
  dbUsername :: ByteString,
  dbPass     :: ByteString,
  dbName     :: ByteString
} deriving (Show)

data State = State
  { wp      :: WpEnv
  -- , db      :: MySQLConn
  , dcToken :: Text
  }

packStr :: String -> ByteString
packStr = encodeUtf8 . pack

parseWpEnv :: IO (Maybe WpEnv)
parseWpEnv = do
  mUrl <- lookupEnv "wpUrl"
  mToken <- lookupEnv "wpToken"
  case (mUrl, mToken) of
    (Just url, Just token) -> pure $ Just $ WpEnv (pack url) (pack token)
    _                      -> pure Nothing

-- parseDbEnv :: IO (Maybe DbEnv)
-- parseDbEnv = do
--     mUrl <- lookupEnv "DB_URL"
--     mUsername <- lookupEnv "DB_USERNAME"
--     mPass <- lookupEnv "DB_PASSWORD"
--     mName <- lookupEnv "DB_NAME"
--     case (mUrl, mUsername, mPass, mName) of
--         (Just url, Just username, Just pass, Just name) ->
--             pure $ Just $ DbEnv url username pass name
--         _ -> pure Nothing

parseWpDbEnv :: IO (Maybe DbEnv)
parseWpDbEnv = do
    mUrl <- lookupEnv "WPDB_HOST"
    mUsername <- lookupEnv "WPDB_USER"
    mPass <- lookupEnv "WPDB_PASS"
    mName <- lookupEnv "WPDB_NAME"
    case (mUrl, mUsername, mPass, mName) of
        (Just url, Just username, Just pass, Just name) ->
            pure $ Just $ DbEnv url (fromString username) (fromString pass) (fromString name)
        _ -> pure Nothing
