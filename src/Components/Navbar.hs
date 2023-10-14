{-# LANGUAGE OverloadedStrings #-}

module Components.Navbar (navBar, isRouteActive) where

import           Data.Text         (Text, isInfixOf)
import           Lucid             (Html, ToHtml (toHtml), aside_, class_, div_,
                                    h2_, nav_)
import           Lucid.Htmx        (hxGet_, hxIndicator_, hxSwap_, hxTarget_)
import           Lucid.Hyperscript (__)
import           Shadcn.Button     (ButtonSize (Small), ButtonVariant (Ghost),
                                    cnButton)

isRouteActive :: Text -> Text -> Bool
isRouteActive "/" "/"          = True
isRouteActive path browserPath = path `isInfixOf` browserPath && (path /= "/")

navItem :: Text -> (Text, Text) -> Html ()
navItem browserPath (text, path) =
  cnButton Ghost Small
    [
      class_ $ "navitem w-full justify-start current:bg-secondary/80" <> if isRouteActive path browserPath then " active" else "",
      hxGet_ path,
      hxSwap_ "innerHTML scroll:top",
      hxTarget_ "#router-outlet",
      hxIndicator_ "#body"
    ]
    $ toHtml text

routes :: [(Text, Text)]
routes = [("Home", "/"), ("Products", "/products"), ("Categories", "/categories")]

navBar :: Text -> Html ()
navBar path =
  aside_ [class_ "sticky top-0 h-screen w-56 bg-background space-y-4 py-4 border-r"] $
    div_ [class_ "flex flex-col mb-4 space-x-1 gap-1"] $ do
      div_ [class_ "px-3 py-2"] $ do
        h2_ [class_ "mb-2 px-4 text-lg font-semibold tracking-tight"] "Morevi"
        nav_ [class_ "space-y-1", __ "on click take .active from .navitem for event.target"] (foldl1 (<>) (fmap (navItem path) routes) )
