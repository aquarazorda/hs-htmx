{-# LANGUAGE OverloadedStrings #-}

module Components.MainWrapper where

import           Components.Navbar     (navBar)
import           Components.Spinner    (spinner)
import           Data.Functor.Identity (Identity)
import           Data.List             (foldl')
import           Data.Text             (Text)
import           Lucid                 (Html, HtmlT, body_, charset_, class_,
                                        content_, crossorigin_, div_, doctype_,
                                        head_, href_, id_, link_, main_, meta_,
                                        name_, rel_, script_, src_)
import           Lucid.Htmx            (useHtmx)
import           Web.Cookie            (CookiesText)

routePage :: CookiesText -> Text -> HtmlT Identity () -> Html ()
routePage cookies path content = do
  doctype_
  head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      useHtmx
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.11"] ("" :: Html ())
      link_ [rel_ "stylesheet", href_ "/public/styles.css"]
      link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
      link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
      link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"]
  body_ [
    class_ $ "font-sans antialiased" <> if isDarkMode then " dark" else "", id_ "body"] $ do
      div_ [class_ "flex flex-col lg:flex-row h-full w-full"] $ do
        navBar path
        spinner "router-loader" "h-screen htmx-request:flex hidden"
        main_ [class_ "flex-1 lg:flex-grow p-6 htmx-request:hidden", id_ "router-outlet"] content
  where
    isDarkMode = foldl' (\acc (name, value) -> acc || (name == "darkMode" && value == "true")) False cookies
