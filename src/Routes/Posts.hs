{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Posts (postsRouter, PostsRouter) where

import Components.Content.Header (contentHeader)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import Data.Functor.Identity (Identity)
import Http (getWpResponse)
import Lucid (Html, HtmlT, class_, div_)
import Router (routePage)
import Servant (Get, Headers, Post, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXPush)
import State (AppM)

type PostsRouter =
  "posts" :> Get '[HTML] (Html ())
    :<|> "posts" :> Post '[HTML] (Headers '[HXPush] (Html ()))

data WpPost = WpPost {id :: Int, name :: String} deriving (Show)

instance FromJSON WpPost where
  parseJSON = withObject "WpPost" $ \v ->
    WpPost
      <$> v .: "id"
      <*> v .: "name"

wcGetPosts :: AppM (Maybe [WpPost])
wcGetPosts = do
  res <- getWpResponse "/products?per_page=1"
  pure $ decode res

content :: HtmlT Identity ()
content = contentHeader "Posts" "" <> div_ [class_ "w-full overflow-auto"] "This is posts page."

type GET = AppM (Html ())

get :: GET
get = do
  posts <- wcGetPosts
  liftIO $ print posts
  pure $ routePage content

type POST = AppM (Headers '[HXPush] (Html ()))

post :: POST
post = pure $ addHeader "/posts" content

postsRouter :: GET :<|> POST
postsRouter = get :<|> post