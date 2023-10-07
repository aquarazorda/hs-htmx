{-# LANGUAGE OverloadedStrings #-}

module Components.Content.Header (contentHeader) where

import Data.Text (Text)
import Lucid (Html, ToHtml (toHtml), class_, div_, h1_)

contentHeader :: Text -> Html () -> Html ()
contentHeader title right =
  div_ [class_ "flex justify-between items-center mb-4"] $
    h1_ [class_ "text-lg font-medium"] $
      toHtml title <> right