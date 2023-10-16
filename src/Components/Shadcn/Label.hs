{-# LANGUAGE OverloadedStrings #-}

module Components.Shadcn.Label where

import           Lucid (Term (termWith), class_)

cnLabel :: Term arg result => arg -> result
cnLabel = termWith "label" [class_ "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70 "]
