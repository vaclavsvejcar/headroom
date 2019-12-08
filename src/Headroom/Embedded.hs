{-|
Module      : Headroom.Embedded
Description : Embedded resource files
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Embedded resource files, using the "Data.FileEmbed" module.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Headroom.Embedded
  ( configFileStub
  )
where

import           Data.FileEmbed                 ( embedStringFile )
import           RIO

-- | Content of dummy /YAML/ configuration file for the application.
configFileStub :: IsString a => a
configFileStub = $(embedStringFile "embedded-data/config-file.yaml")
