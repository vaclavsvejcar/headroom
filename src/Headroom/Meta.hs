{-|
Module      : Headroom.Meta
Description : Application metadata
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Metadata about the application (e.g. build version, application name, etc).
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Meta
  ( buildVer
  )
where

import           Data.Version                   ( showVersion )
import           Paths_headroom                 ( version )
import           RIO

-- | Returns application version, as specified in @headroom.cabal@ file.
buildVer :: String
buildVer = showVersion version
