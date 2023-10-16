{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Textarea where

import           Lucid (Term (termWith), class_)

cnTextarea :: Term arg result => arg -> result
cnTextarea = termWith "textarea" [class_ "flex min-h-[60px] w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 "]
