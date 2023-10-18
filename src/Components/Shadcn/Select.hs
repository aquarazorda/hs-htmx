{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Select where
import           Lucid (Term (termWith), class_)

cnSelect :: Term arg result => arg -> result
cnSelect = termWith "select" [class_ "flex h-10 items-center justify-between rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background placeholder:text-muted-foreground focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"]
