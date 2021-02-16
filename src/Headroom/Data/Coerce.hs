{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Data.Coerce
  ( coerce
  , inner
  )
where

import           Data.Coerce                         ( Coercible
                                                     , coerce
                                                     )
import           RIO


inner :: (Coercible a b) => (b -> b) -> a -> a
inner f = coerce . f . coerce
