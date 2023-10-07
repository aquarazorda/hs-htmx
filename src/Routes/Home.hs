{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Home (homeRouter, HomeRouter) where

import Data.Functor.Identity (Identity)
import Lucid (Html, HtmlT, div_)
import Router (routePage)
import Servant (Handler, Headers, addHeader, (:<|>)(..), Get, Post, Server)
import Servant.Htmx (HXPush)
import Servant.HTML.Lucid (HTML)

type HomeRouter = Get '[HTML] (Html ())
    :<|> Post '[HTML] (Headers '[HXPush] (Html ()))

content :: HtmlT Identity ()
content = "This is home page."

get :: Handler (Html ())
get = pure $ routePage content

post :: Handler (Headers '[HXPush] (Html ()))
post = pure $ addHeader "/" content

homeRouter :: Server HomeRouter
homeRouter = get :<|> post