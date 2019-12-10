{-|
Module      : Headroom.Header.Impl
Description : All license header implementations
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Re-exports @headerSizeXY@ functions from all existing implementation modules.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header.Impl
  ( headerSizeHaskell
  , headerSizeHTML
  , headerSizeJava
  , headerSizeScala
  )
where

import           Headroom.Header.Impl.Haskell   ( headerSizeHaskell )
import           Headroom.Header.Impl.HTML      ( headerSizeHTML )
import           Headroom.Header.Impl.Java      ( headerSizeJava )
import           Headroom.Header.Impl.Scala     ( headerSizeScala )
