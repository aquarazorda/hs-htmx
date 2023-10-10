{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Postgres.Category where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Field, SqlInt4, SqlText)

data Category' a b c = Category
  { id   :: a
  , name :: b
  , slug :: c
  }
type Category = Category' Int String String
type CategoryField = Category' (Field SqlInt4) (Field SqlText) (Field SqlText)

$(makeAdaptorAndInstance "pCategory" ''Category')
