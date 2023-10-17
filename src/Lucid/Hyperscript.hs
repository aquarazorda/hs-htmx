{-# LANGUAGE OverloadedStrings #-}

module Lucid.Hyperscript (__, withAutoFocus) where

import           Data.Text  (Text)
import           Lucid.Base (Attribute, makeAttribute)

__ :: Text -> Attribute
__ = makeAttribute "_"

withAutoFocus :: Bool -> Attribute
withAutoFocus True  = __ "init wait 0.2s then go to the center of me smoothly then toggle [@data-state=selected] on me for 2s"
withAutoFocus False = makeAttribute "" ""
