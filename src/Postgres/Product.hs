{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Postgres.Product where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Field, SqlFloat8, SqlInt4, SqlText)
import           Postgres.Category          (Category)
import           Postgres.Label             (Label)
import           Postgres.Track             (Track)

data Product' a b c d e f g h i j = Product
  { id          :: a,
    name        :: b,
    description :: c,
    image_url   :: d,
    categories  :: e,
    price       :: f,
    discogsId   :: g,
    tracks      :: h,
    year        :: i,
    label       :: j
  }

type Product = Product' Int String (Maybe String) String Category Double Int (Maybe [Track]) Int (Maybe Label)
type ProductField = Product' (Field SqlInt4) (Field SqlText) (Maybe (Field SqlText)) (Field SqlText) (Field SqlInt4) (Field SqlFloat8) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4)

$(makeAdaptorAndInstance "pProduct" ''Product')
