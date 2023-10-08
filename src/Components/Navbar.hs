{-# LANGUAGE OverloadedStrings #-}

module Components.Navbar (navBar) where

import Data.Text (Text)
import Lucid
  ( Html,
    ToHtml (toHtml),
    aside_,
    button_,
    class_,
    div_,
    h1_,
    nav_,
  )
import Lucid.Htmx (hxSwap_, hxTarget_, hxGet_, hxIndicator_)

navItem :: Text -> Text -> Html ()
navItem path text =
  button_
    [ class_ "w-full flex items-center space-x-2 hover:bg-gray-200 active:bg-gray-300 py-2 px-2 rounded-lg text-gray-500",
      hxGet_ path,
      hxSwap_ "innerHTML scroll:top",
      hxTarget_ "#router-outlet",
      hxIndicator_ "#body"
    ]
    $ h1_ [class_ "text-sm font-medium"]
    $ toHtml text

navBar :: Html ()
navBar =
  aside_ [class_ "sticky top-0 h-screen w-56 bg-gray-100 text-gray-800 p-4"] $
    div_ [class_ "flex items-center mb-4 space-x-1"] (h1_ [class_ "text-lg font-medium"] "HS-WEB")
      <> nav_
        [class_ "space-y-2"]
        (navItem "/" "Home" <> navItem "/products" "Products")
