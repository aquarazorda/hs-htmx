{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Toggle where

import           Data.Text         (Text, unwords)
import           Lucid             (Attribute, Html, button_, class_, input_,
                                    name_, type_, value_)
import           Lucid.Hyperscript (__)
import           Prelude           hiding (unwords)

data ToggleVariant = Default | Outline
data ToggleSize = DefaultSize | Small | Large

cnToggle :: ToggleVariant -> ToggleSize -> Text -> Text -> [Attribute] -> Html () -> Html ()
cnToggle variant size val name attrs children = do
  button_ (attrs <> [
    __ "on click toggle [@data-state=on] then toggle [@checked=true] on the <input/> in me ",
    class_ $ unwords [" inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors hover:bg-muted hover:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:pointer-events-none disabled:opacity-50 data-[state=on]:bg-accent data-[state=on]:text-accent-foreground", s, v]
    ]) $ do
      children
      input_  [ class_ " hidden", type_ "checkbox", value_ val,  name_ name]
    where
      v = case variant of
        Default -> "bg-transparent"
        Outline -> "border border-input bg-transparent shadow-sm hover:bg-accent hover:text-accent-foreground"
      s = case size of
        DefaultSize -> "h-9 px-3"
        Small       -> "h-8 px-2"
        Large       -> "h-10 px-3"
