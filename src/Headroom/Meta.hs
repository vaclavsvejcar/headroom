{-|
Module      : Headroom.Meta
Description : Application metadata (name, vendor, etc.)
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing application metadata, such as application name, vendor,
version, etc.
-}
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
