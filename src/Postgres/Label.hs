{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Postgres.Label where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Field, FieldNullable, SqlInt4,
                                             SqlText)

data Label' a b c d = Label
  { id        :: a
  , name      :: b
  , discogsId :: c
  , image_url :: d
  }
type Label = Label' Int String Int (Maybe String)
type LabelField = Label' (Field SqlInt4) (Field SqlText) (Field SqlInt4) (FieldNullable SqlText)

$(makeAdaptorAndInstance "pLabel" ''Label')
