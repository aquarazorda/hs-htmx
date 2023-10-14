{-# LANGUAGE OverloadedStrings #-}

module Shadcn.Input where

import           Lucid      (class_, input_)
import           Lucid.Base (Attribute, Html)

cnInput :: [Attribute] -> Html ()
cnInput attrs = input_ $
  [ class_ "flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"]
  <> attrs
