{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Posts (postsRouter, PostsRouter) where

import Data.Functor.Identity (Identity)
import Lucid (Html, HtmlT, div_)
import Router (routePage)
import Servant (Handler, Headers, addHeader, (:>), (:<|>)(..), Get, Post, Server)
import Servant.Htmx (HXPush)
import Servant.HTML.Lucid (HTML)

type PostsRouter = "posts" :> Get '[HTML] (Html ())
    :<|> "posts" :> Post '[HTML] (Headers '[HXPush] (Html ()))

content :: HtmlT Identity ()
content = "This is posts page."

get :: Handler (Html ())
get = pure $ routePage content

post :: Handler (Headers '[HXPush] (Html ()))
post = pure $ addHeader "/posts" $ div_ content

postsRouter :: Server PostsRouter
postsRouter = get :<|> post