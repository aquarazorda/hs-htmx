{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Posts (postsRouter, PostsRouter) where

import Components.Content.Header (contentHeader)
import Data.Aeson (decode)
import Http (getWpResponse)
import Lucid (Html, class_, div_, ToHtml (toHtml))
import Router (routePage)
import Servant (Get, Headers, Post, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXPush)
import State (AppM)
import Data.WC.Product (WpPost(..))

type PostsRouter =
  "posts" :> Get '[HTML] (Html ())
    :<|> "posts" :> Post '[HTML] (Headers '[HXPush] (Html ()))

wcGetPosts :: AppM (Maybe [WpPost])
wcGetPosts = do
  res <- getWpResponse "/products"
  pure $ decode res

content :: AppM (Html ())
content = do
    maybe contentEmpty withPosts <$> wcGetPosts
    where
        wrapper :: Html() -> Html()
        wrapper h = contentHeader "Posts" "" <> div_ [class_ "w-full overflow-auto"] h
        withPosts :: [WpPost] -> Html ()
        withPosts d = wrapper $ foldl1 (<>) $ fmap postItem d
        contentEmpty :: Html ()
        contentEmpty = wrapper "No posts found."

postItem :: WpPost -> Html ()
postItem (WpPost { name = cName }) = div_ $ toHtml cName

type GET = AppM (Html ())

get :: GET
get = routePage <$> content

type POST = AppM (Headers '[HXPush] (Html ()))

post :: POST
post = addHeader "/posts" <$> content

postsRouter :: GET :<|> POST
postsRouter = get :<|> post