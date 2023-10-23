{-# LANGUAGE OverloadedStrings #-}

module Components.Spinner (spinner) where

import           Data.Text (Text)
import           Lucid     (Html, class_, div_, id_, role_, span_)
import           Prelude   hiding (id)

spinner :: Text -> Text -> Html ()
spinner id cls =
  div_ [class_ $ "w-full items-center justify-center " <> cls, id_ id] $
  div_
    [ class_ "inline-block h-8 w-8 animate-spin rounded-full border-4 border-solid border-current border-r-transparent align-[-0.125em] motion-reduce:animate-[spin_1.5s_linear_infinite]",
      role_ "status"
    ]
    (span_ [class_ "!absolute !-m-px !h-px !w-px !overflow-hidden !whitespace-nowrap !border-0 !p-0 ![clip:rect(0,0,0,0)]"] "Loading")
