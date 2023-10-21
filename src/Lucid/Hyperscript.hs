{-# LANGUAGE OverloadedStrings #-}

module Lucid.Hyperscript (__, withAutoFocus) where

import           Data.Text  (Text)
import           Lucid.Base (Attribute, makeAttribute)

__ :: Text -> Attribute
__ = makeAttribute "_"

withAutoFocus :: Attribute
withAutoFocus = __ "init wait 0.2s then if $focusId get #{$focusId} then if it go to the center of it smoothly then toggle [@data-state=selected] on it for 2s\
\ end end then set $focusId to null"
