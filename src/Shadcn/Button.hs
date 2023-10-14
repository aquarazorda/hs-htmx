{-# LANGUAGE OverloadedStrings #-}

module Shadcn.Button where

import           Lucid (Term (termWith), class_)

data ButtonVariant = Destructive | Outline | Secondary | Ghost | Link
data ButtonSize = Small | Medium | Large

cnButton :: Term arg result => Maybe ButtonVariant -> Maybe ButtonSize -> arg -> result
cnButton mv ms = termWith "button" [class_ $
    "inline-flex items-center justify-center rounded-md text-sm font-medium ring-offset-background transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50"
    <> appendClass <> appendSize]
  where
    appendClass = case mv of
      Nothing -> " bg-primary text-primary-foreground hover:bg-primary/90"
      Just variant -> case variant of
        Destructive -> " bg-destructive text-destructive-foreground hover:bg-destructive/90"
        Outline -> " border border-input bg-background hover:bg-accent hover:text-accent-foreground"
        Secondary -> " bg-secondary text-secondary-foreground hover:bg-secondary/80"
        Ghost -> " hover:bg-accent hover:text-accent-foreground"
        Link -> " text-primary underline-offset-4 hover:underline"
    appendSize = case ms of
      Nothing -> " h-10 px-4 py-2"
      Just size -> case size of
        Small  -> " h-9 rounded-md px-3"
        Medium -> " h-11 rounded-md px-8"
        Large  -> " h-10 w-10"

cnBtn :: Term arg result => arg -> result
cnBtn = cnButton Nothing Nothing
