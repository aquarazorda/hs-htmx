{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Button where

import           Data.Text (unwords)
import           Lucid     (Attribute, Term (termWith), class_, disabled_)
import           Prelude   hiding (unwords)

data ButtonVariant = Default | Destructive | Outline | Secondary | Ghost | Link
data ButtonSize = DefaultSize | Small | Medium | Large

disableWhen :: Bool -> [Attribute] -> [Attribute]
disableWhen True attrs  = attrs <> [disabled_ ""]
disableWhen False attrs = attrs

cnButton :: Term arg result => Maybe ButtonVariant -> Maybe ButtonSize -> arg -> result
cnButton mv ms = termWith "button" [class_ $
    unwords [appendClass, appendSize, "inline-flex items-center rounded-md text-sm font-medium ring-offset-background transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50 "]]
  where
    appendClass = case mv of
      Just c -> case c of
        Default -> "bg-primary text-primary-foreground hover:bg-primary/90"
        Destructive -> "bg-destructive text-destructive-foreground hover:bg-destructive/90"
        Outline -> "border border-input bg-background hover:bg-accent hover:text-accent-foreground"
        Secondary -> "bg-secondary text-secondary-foreground hover:bg-secondary/80"
        Ghost -> "hover:bg-accent hover:text-accent-foreground"
        Link -> "text-primary underline-offset-4 hover:underline"
      _ -> ""
    appendSize = case ms of
      Just s -> case s of
        DefaultSize -> "h-10 px-4 py-2"
        Small       -> "h-9 rounded-md px-3"
        Medium      -> "h-11 rounded-md px-8"
        Large       -> "h-10 w-10"
      _ -> ""

cnBtn :: Term arg result => arg -> result
cnBtn = cnButton (Just Default) (Just Small)
