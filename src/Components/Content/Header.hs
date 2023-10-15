{-# LANGUAGE OverloadedStrings #-}

module Components.Content.Header (contentHeader) where

import           Data.Maybe (fromMaybe)
import           Lucid      (Html, class_, div_, h2_, p_)

contentHeader :: Html () -> Maybe (Html ()) -> Html ()
contentHeader title rightElement = div_ [class_ "flex-1 flex-col p-2 md:flex"] $ do
  h2_ [class_ "text-2xl font-bold tracking-tight"] title
  p_ [class_ "text-muted-foreground"] $ fromMaybe "" rightElement

