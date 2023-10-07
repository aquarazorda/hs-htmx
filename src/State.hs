module State where

import Control.Monad.Trans.Reader (ReaderT, ask)
import Servant (Handler)
import Data.Text (Text, pack)
import Configuration.Dotenv (loadFile, defaultConfig)
import Configuration.Dotenv.Environment (lookupEnv)

type AppM = ReaderT State Handler

data Env = Env
  { wpUrl :: Text,
    wpToken :: Text
  } deriving (Show)

data State = State
  { env :: Env
  } deriving (Show)

getEnv :: AppM Env
getEnv = env <$> ask

parseEnv :: IO (Maybe Env)
parseEnv = do
  loadFile defaultConfig
  mUrl <- lookupEnv "wpUrl"
  mToken <- lookupEnv "wpToken"
  case (mUrl, mToken) of
    (Just url, Just token) -> pure $ Just $ Env (pack url) (pack token)
    _ -> pure Nothing