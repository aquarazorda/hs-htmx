{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Badge where
import           Lucid (Term (termWith), class_)

data BadgeVariant = Default | Secondary | Destructive | Outline

cnBadge :: Term arg result => BadgeVariant -> arg -> result
cnBadge variant = termWith "div" [class_ $ "inline-flex items-center rounded-md border px-2.5 py-0.5 text-xs font-semibold transition-colors focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 " <> appendClass <> " "]
  where
    appendClass = case variant of
      Default -> "border-transparent bg-primary text-primary-foreground shadow hover:bg-primary/80"
      Secondary -> "border-transparent bg-secondary text-secondary-foreground hover:bg-secondary/80"
      Destructive -> "border-transparent bg-destructive text-destructive-foreground shadow hover:bg-destructive/80"
      Outline -> "text-foreground"

cnBadge_ :: Term arg result => arg -> result
cnBadge_ = cnBadge Default
