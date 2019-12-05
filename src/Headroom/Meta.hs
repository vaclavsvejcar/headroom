{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Meta
  ( buildVer
  )
where

import           Data.Version                   ( showVersion )
import           Paths_headroom                 ( version )
import           RIO

buildVer :: String
buildVer = showVersion version
