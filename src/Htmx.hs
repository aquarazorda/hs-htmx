{-# LANGUAGE OverloadedStrings #-}

module Htmx where

import           Data.Text  (Text)
import           Lucid.Base (Attribute, makeAttribute)
import           Lucid.Htmx (hxGet_, hxIndicator_, hxSwap_, hxTarget_)

hxDisinherit_ :: Text -> Attribute
hxDisinherit_ = makeAttribute "data-hx-disinherit"

navChangeAttrs :: Text -> [Attribute]
navChangeAttrs path = [
  hxGet_ path,
  hxSwap_ "innerHTML scroll:top",
  hxTarget_ "#router-outlet",
  hxIndicator_ "#body"
  ]
