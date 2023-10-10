{-# LANGUAGE OverloadedStrings #-}

module Components.Content.Header (contentHeader) where

import           Data.Text (Text)
import           Lucid     (Html, ToHtml (toHtml), class_, div_, h1_)

contentHeader :: Text -> Maybe (Html ()) -> Html ()
contentHeader title rightElement =
  div_ [class_ "flex justify-between items-center mb-4"] $
    h1_ [class_ "text-lg font-medium"] $
      case rightElement of
        Nothing -> toHtml title
        Just r  -> toHtml title <> r
