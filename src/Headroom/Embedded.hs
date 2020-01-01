{-|
Module      : Headroom.Embedded
Description : Embedded resource files
Copyright   : (c) 2019-2020 Vaclav Svejcar
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
    CSS     -> $(embedStringFile "embedded/license/apache2/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/apache2/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/apache2/html.mustache")
    Java    -> $(embedStringFile "embedded/license/apache2/java.mustache")
    JS      -> $(embedStringFile "embedded/license/apache2/js.mustache")
    Scala   -> $(embedStringFile "embedded/license/apache2/scala.mustache")
  BSD3 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/bsd3/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/bsd3/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/bsd3/html.mustache")
    Java    -> $(embedStringFile "embedded/license/bsd3/java.mustache")
    JS      -> $(embedStringFile "embedded/license/bsd3/js.mustache")
    Scala   -> $(embedStringFile "embedded/license/bsd3/scala.mustache")
  GPL2 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/gpl2/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/gpl2/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/gpl2/html.mustache")
    Java    -> $(embedStringFile "embedded/license/gpl2/java.mustache")
    JS      -> $(embedStringFile "embedded/license/gpl2/js.mustache")
    Scala   -> $(embedStringFile "embedded/license/gpl2/scala.mustache")
  GPL3 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/gpl3/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/gpl3/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/gpl3/html.mustache")
    Java    -> $(embedStringFile "embedded/license/gpl3/java.mustache")
    JS      -> $(embedStringFile "embedded/license/gpl3/js.mustache")
    Scala   -> $(embedStringFile "embedded/license/gpl3/scala.mustache")
  MIT -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/mit/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/mit/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/mit/html.mustache")
    Java    -> $(embedStringFile "embedded/license/mit/java.mustache")
    JS      -> $(embedStringFile "embedded/license/mit/js.mustache")
    Scala   -> $(embedStringFile "embedded/license/mit/scala.mustache")
