{-# LANGUAGE OverloadedStrings #-}

module Router (routePage) where
  
import Lucid (HtmlT, Html, head_, link_, href_, rel_, id_, class_, div_, body_)
import Data.Functor.Identity (Identity)
import Lucid.Htmx (useHtmx)
import Navbar (navBar)

routePage :: HtmlT Identity () -> Html ()
routePage content = head_ (useHtmx <> link_ [href_ "/public/styles.css", rel_ "stylesheet"])
    <> body_ [class_ "flex"] navBar <> div_ [class_ "flex-grow", id_ "router-outlet"] content