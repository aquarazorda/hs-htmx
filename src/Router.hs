{-# LANGUAGE OverloadedStrings #-}

module Router (routePage) where

import Components.Navbar (navBar)
import Data.Functor.Identity (Identity)
import Lucid (Html, HtmlT, body_, class_, head_, href_, id_, link_, main_, rel_)
import Lucid.Htmx (useHtmx)

routePage :: HtmlT Identity () -> Html ()
routePage content =
  head_ (useHtmx <> link_ [href_ "/public/styles.css", rel_ "stylesheet"])
    <> body_ [class_ "flex"] navBar
    <> main_ [class_ "flex-grow p-6", id_ "router-outlet"] content