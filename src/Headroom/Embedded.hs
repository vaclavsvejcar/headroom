{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Headroom.Embedded
  ( configFileStub
  )
where

import           Data.FileEmbed                 ( embedStringFile )
import           RIO

configFileStub :: IsString a => a
configFileStub = $(embedStringFile "embedded-data/config-file.yaml")
