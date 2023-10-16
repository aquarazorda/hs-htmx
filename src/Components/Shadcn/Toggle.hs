{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Toggle where

import           Lucid (Term (termWith), class_)

data ToggleVariant = Default | Outline
data ToggleSize = DefaultSize | Small | Large

cnToggle :: Term arg result => ToggleVariant -> ToggleSize -> arg -> result
cnToggle variant size = termWith "button" [
  class_ $ "inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors hover:bg-muted hover:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:pointer-events-none disabled:opacity-50 data-[state=on]:bg-accent data-[state=on]:text-accent-foreground "
    <> v <> " " <> s <> " "
  ]
  where
    v = case variant of
      Default -> "bg-transparent"
      Outline -> "border border-input bg-transparent shadow-sm hover:bg-accent hover:text-accent-foreground"
    s = case size of
      DefaultSize -> "h-9 px-3"
      Small       -> "h-8 px-2"
      Large       -> "h-10 px-3"
