{-# LANGUAGE OverloadedStrings #-}
module Lucid.Hyperscript (__) where

import           Data.Text  (Text)
import           Lucid.Base (Attribute, makeAttribute)

__ :: Text -> Attribute
__ = makeAttribute "_"
