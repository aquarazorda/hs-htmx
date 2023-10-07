{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Posts (postsRouter, PostsRouter) where

import Data.Functor.Identity (Identity)
import Lucid (Html, HtmlT, div_, class_)
import Router (routePage)
import Servant (Handler, Headers, addHeader, (:>), (:<|>)(..), Get, Post, Server)
import Servant.Htmx (HXPush)
import Servant.HTML.Lucid (HTML)
import Components.Content.Header (contentHeader)

type PostsRouter = "posts" :> Get '[HTML] (Html ())
    :<|> "posts" :> Post '[HTML] (Headers '[HXPush] (Html ()))

content :: HtmlT Identity ()
content = contentHeader "Posts" "" <> div_ [class_ "w-full overflow-auto"] "This is posts page."

get :: Handler (Html ())
get = pure $ routePage content

post :: Handler (Headers '[HXPush] (Html ()))
post = pure $ addHeader "/posts" content

postsRouter :: Server PostsRouter
postsRouter = get :<|> post