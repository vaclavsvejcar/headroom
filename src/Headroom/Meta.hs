{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Meta
  ( TemplateType
  , buildVer
  )
where

import           Data.Version                   ( showVersion )
import           Headroom.Template.Mustache     ( Mustache )
import           Paths_headroom                 ( version )
import           RIO


-- | Type of the template format used for license headers.
type TemplateType = Mustache

-- | Returns application version, as specified in @headroom.cabal@ file.
buildVer :: String
buildVer = showVersion version
