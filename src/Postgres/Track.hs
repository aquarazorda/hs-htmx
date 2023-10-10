{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Postgres.Track where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Field, FieldNullable, SqlInt4,
                                             SqlText)

data Track' a b c d = Track
  { id       :: a
  , name     :: b
  , artistId :: c
  , url      :: d
  }
type Track = Track' Int String Int (Maybe String)
type TrackField = Track' (Field SqlInt4) (Field SqlText) (Field SqlInt4) (FieldNullable SqlText)

$(makeAdaptorAndInstance "pTrack" ''Track')
