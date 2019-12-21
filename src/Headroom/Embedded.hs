{-|
Module      : Headroom.Embedded
Description : Embedded resource files
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Contains resources that were embedded from the @embedded/@ source folder during
compile time, using the "Data.FileEmbed" module.
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

-- | License template for given 'License'.
licenseTemplate :: IsString a
                => License -- ^ 'License' for which to return the template
                -> a       -- ^ template text
licenseTemplate (License licenseType fileType) = case licenseType of
  Apache2 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/apache2/css.jinja2")
    Haskell -> $(embedStringFile "embedded/license/apache2/haskell.jinja2")
    HTML    -> $(embedStringFile "embedded/license/apache2/html.jinja2")
    Java    -> $(embedStringFile "embedded/license/apache2/java.jinja2")
    JS      -> $(embedStringFile "embedded/license/apache2/js.jinja2")
    Scala   -> $(embedStringFile "embedded/license/apache2/scala.jinja2")
  BSD3 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/bsd3/css.jinja2")
    Haskell -> $(embedStringFile "embedded/license/bsd3/haskell.jinja2")
    HTML    -> $(embedStringFile "embedded/license/bsd3/html.jinja2")
    Java    -> $(embedStringFile "embedded/license/bsd3/java.jinja2")
    JS      -> $(embedStringFile "embedded/license/bsd3/js.jinja2")
    Scala   -> $(embedStringFile "embedded/license/bsd3/scala.jinja2")
  GPL2 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/gpl2/css.jinja2")
    Haskell -> $(embedStringFile "embedded/license/gpl2/haskell.jinja2")
    HTML    -> $(embedStringFile "embedded/license/gpl2/html.jinja2")
    Java    -> $(embedStringFile "embedded/license/gpl2/java.jinja2")
    JS      -> $(embedStringFile "embedded/license/gpl2/js.jinja2")
    Scala   -> $(embedStringFile "embedded/license/gpl2/scala.jinja2")
  GPL3 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/gpl3/css.jinja2")
    Haskell -> $(embedStringFile "embedded/license/gpl3/haskell.jinja2")
    HTML    -> $(embedStringFile "embedded/license/gpl3/html.jinja2")
    Java    -> $(embedStringFile "embedded/license/gpl3/java.jinja2")
    JS      -> $(embedStringFile "embedded/license/gpl3/js.jinja2")
    Scala   -> $(embedStringFile "embedded/license/gpl3/scala.jinja2")
  MIT -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/mit/css.jinja2")
    Haskell -> $(embedStringFile "embedded/license/mit/haskell.jinja2")
    HTML    -> $(embedStringFile "embedded/license/mit/html.jinja2")
    Java    -> $(embedStringFile "embedded/license/mit/java.jinja2")
    JS      -> $(embedStringFile "embedded/license/mit/js.jinja2")
    Scala   -> $(embedStringFile "embedded/license/mit/scala.jinja2")
