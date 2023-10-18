{-# LANGUAGE OverloadedStrings #-}

module Components.Navbar (navBar, isRouteActive, navChangeAttrs) where

import           Components.Icons         (foldersIcon, vortexIcon)
import           Components.Shadcn.Button (ButtonSize (ButtonSmall),
                                           ButtonVariant (ButtonDefault, ButtonGhost),
                                           cnButton)
import           Data.Text                (Text, isInfixOf)
import           Lucid                    (Attribute, Html, ToHtml (toHtml),
                                           aside_, class_, div_, h2_, id_, nav_)
import           Lucid.Htmx               (hxGet_, hxIndicator_, hxSwap_,
                                           hxTarget_)
import           Lucid.Hyperscript        (__)

type SubItem = (Text, Text, [Attribute] -> Html ())
type MenuItem = (Text, [SubItem])

menuItems :: [MenuItem]
menuItems = [
    ("Morevi", [("Home", "/", vortexIcon)
    -- ("Categories", "/categories", categoryIcon)
    ])
  -- , ("WooCommerce", [("Products", "/products", productIcon)])
  , ("Discogs", [("Folders", "/folders", foldersIcon)])
  ]

isRouteActive :: Text -> Text -> Bool
isRouteActive "/" "/"          = True
isRouteActive path browserPath = path `isInfixOf` browserPath && (path /= "/")

navChangeAttrs :: Text -> [Attribute]
navChangeAttrs path = [
  hxGet_ path,
  hxSwap_ "innerHTML scroll:top",
  hxTarget_ "#router-outlet",
  hxIndicator_ "#body"
  ]

navItem :: Text -> SubItem -> Html ()
navItem browserPath (text, path, icon) =
  cnButton (Just ButtonGhost) (Just ButtonSmall)
    ([ class_ $ "navitem w-full justify-start current:bg-secondary/80" <> if isRouteActive path browserPath then " active" else "" ]
      <> navChangeAttrs path) $ do
      icon [class_ "mr-2 h-4 w-4"]
      toHtml text

navBar :: Text -> Html ()
navBar path =
  aside_ [class_ "flex flex-col lg:sticky lg:top-0 lg:h-screen lg:w-48 bg-background py-4 border-r flex-shrink-0 lg:flex-none"] $ do
    div_ [class_ "flex lg:flex-col mb-4 gap-4"] $
      foldl1 (<>) $ fmap (\(text, items) -> do
        div_ [class_ "px-3 py-2"] $ do
          h2_ [class_ "mb-2 px-4 text-lg font-semibold tracking-tight"] $ toHtml text
          nav_ [class_ "space-y-1", __ "on click take .active from .navitem for event.target"] (foldl1 (<>) (fmap (navItem path) items) )
        ) menuItems
    div_ [class_ "px-4 mt-auto"] $
      cnButton (Just ButtonDefault) (Just ButtonSmall) [
        class_ "p-4 w-full justify-center",
        id_ "theme-toggler",
        __ "init if cookies['darkMode'] is 'true' then put 'Light ☀️' into my innerHTML else put 'Dark 🌙' into my innerHTML end on click if my innerHTML is 'Light ☀️' then put 'Dark 🌙' into my innerHTML then remove .dark from body then set cookies['darkMode'] to {value: 'false', maxAge: 34560000} else put 'Light ☀️' into my innerHTML then add .dark to body then set cookies['darkMode'] to {value: 'true', maxAge: 34560000}"
      ] ""
