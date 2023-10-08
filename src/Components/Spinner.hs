{-# LANGUAGE OverloadedStrings #-}

module Components.Spinner (spinner) where

import Prelude hiding (id)
import Lucid (Html, class_, div_, role_, span_, id_)
import Data.Text (Text)

spinner :: Text -> Text -> Html ()
spinner id cls =
  div_ [class_ $ "w-full h-screen items-center justify-center " <> cls, id_ id] $
  div_
    [ class_ "inline-block h-8 w-8 animate-spin rounded-full border-4 border-solid border-current border-r-transparent align-[-0.125em] motion-reduce:animate-[spin_1.5s_linear_infinite]",
      role_ "status"
    ]
    (span_ [class_ "!absolute !-m-px !h-px !w-px !overflow-hidden !whitespace-nowrap !border-0 !p-0 ![clip:rect(0,0,0,0)]"] "Loading")