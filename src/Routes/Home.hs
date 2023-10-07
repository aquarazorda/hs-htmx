{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Home (homeRouter, HomeRouter) where

import Data.Functor.Identity (Identity)
import Lucid (Html, HtmlT)
import Router (routePage)
import Servant (Headers, addHeader, (:<|>)(..), Get, Post)
import Servant.Htmx (HXPush)
import Servant.HTML.Lucid (HTML)
import State (AppM)

type HomeRouter = Get '[HTML] (Html ())
    :<|> Post '[HTML] (Headers '[HXPush] (Html ()))

content :: HtmlT Identity ()
content = "This is home page."

type GET = AppM (Html ())

get :: GET
get = pure $ routePage content

type POST = AppM (Headers '[HXPush] (Html ()))

post :: POST
post = pure $ addHeader "/" content

homeRouter :: GET :<|> POST
homeRouter = get :<|> post