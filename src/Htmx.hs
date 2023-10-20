{-# LANGUAGE OverloadedStrings #-}

module Htmx where

import           Data.Text  (Text)
import           Lucid.Base (Attribute, makeAttribute)

hxDisinherit_ :: Text -> Attribute
hxDisinherit_ = makeAttribute "data-hx-disinherit"
