{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header.All
  ( headerSizeHaskell
  , headerSizeScala
  )
where

import           Headroom.Header.Impl.Haskell   ( headerSizeHaskell )
import           Headroom.Header.Impl.Scala     ( headerSizeScala )
