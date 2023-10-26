{-# LANGUAGE OverloadedStrings #-}

module Data.Nav where

import Components.Icons (Icon, foldersIcon, plusIcon, vortexIcon)
import Data.Text (Text)

menuItems :: [(Text, [(Text, Text, Icon)])]
menuItems =
  [
    ( "Morevi"
    ,
      [ ("Home", "/", vortexIcon)
      -- ("Categories", "/categories", categoryIcon)
      ]
    )
  , -- , ("WooCommerce", [("Products", "/products", productIcon)])

    ( "Discogs"
    ,
      [ ("Add new", "add-product", plusIcon)
      , ("Folders", "folders", foldersIcon)
      ]
    )
  ]
