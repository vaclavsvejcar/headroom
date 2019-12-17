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
  , licenseTemplate
  )
where

import           Data.FileEmbed                 ( embedStringFile )
import           Headroom.FileType              ( FileType(..) )
import           Headroom.License               ( License(..)
                                                , LicenseType(..)
                                                )
import           RIO

-- | Content of dummy /YAML/ configuration file for the application.
configFileStub :: IsString a => a
configFileStub = $(embedStringFile "embedded/config-file.yaml")

licenseTemplate :: IsString a => License -> a
licenseTemplate (License BSD3 fileType) = bsd3 fileType
 where
  bsd3 filetype = case filetype of
    CSS     -> $(embedStringFile "embedded/license/bsd3-css.jinja2")
    Haskell -> $(embedStringFile "embedded/license/bsd3-haskell.jinja2")
    HTML    -> $(embedStringFile "embedded/license/bsd3-html.jinja2")
    Java    -> $(embedStringFile "embedded/license/bsd3-java.jinja2")
    JS      -> $(embedStringFile "embedded/license/bsd3-js.jinja2")
    Scala   -> $(embedStringFile "embedded/license/bsd3-scala.jinja2")

