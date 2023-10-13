{-# LANGUAGE OverloadedStrings #-}

module Components.Navbar (navBar, isRouteActive) where

import           Data.Text         (Text, isInfixOf)
import           Lucid             (Html, ToHtml (toHtml), aside_, button_,
                                    class_, div_, h1_, nav_)
import           Lucid.Htmx        (hxGet_, hxIndicator_, hxSwap_, hxTarget_)
import           Lucid.Hyperscript (__)

isRouteActive :: Text -> Text -> Bool
isRouteActive "/" "/"          = True
isRouteActive path browserPath = path `isInfixOf` browserPath && (path /= "/")

navItem :: Text -> (Text, Text) -> Html ()
navItem browserPath (text, path) =
  button_
    [ class_ (if isRouteActive path browserPath then "active" <> " " <> buttonClass else buttonClass),
      hxGet_ path,
      hxSwap_ "innerHTML scroll:top",
      hxTarget_ "#router-outlet",
      hxIndicator_ "#body"
    ]
    $ h1_ [class_ "text-sm font-medium"]
    $ toHtml text
  where
    buttonClass = "navitem w-full flex items-center space-x-2 hover:bg-gray-200 current:bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-500"

routes :: [(Text, Text)]
routes = [("Home", "/"), ("Products", "/products"), ("Categories", "/categories")]

navBar :: Text -> Html ()
navBar path =
  aside_ [class_ "sticky top-0 h-screen w-56 bg-gray-100 text-gray-800 p-4"] $
    div_ [class_ "flex items-center mb-4 space-x-1"] (h1_ [class_ "text-lg font-medium"] "HS-WEB")
      <> nav_
        [class_ "space-y-2", __ "on click take .active from .navitem for event.target"]
        (foldl1 (<>) (fmap (navItem path) routes) )
