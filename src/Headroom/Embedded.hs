{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Headroom.Embedded
  ( configFileStub
  , defaultConfig
  , licenseTemplate
  )
where

import           Data.FileEmbed                 ( embedStringFile )
import           Headroom.Types                 ( FileType(..)
                                                , LicenseType(..)
                                                )
import           RIO


-- | Content of dummy /YAML/ configuration file for the application.
configFileStub :: IsString a => a
configFileStub = $(embedStringFile "embedded/config-file.yaml")

defaultConfig :: IsString a => a
defaultConfig = $(embedStringFile "embedded/default-config.yaml")

-- | License template for given 'License'.
licenseTemplate :: IsString a
                => LicenseType -- ^ 'License' for which to return the template
                -> FileType  -- ^ 'License' for which to return the template
                -> a       -- ^ template text
licenseTemplate licenseType fileType = case licenseType of
  Apache2 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/apache2/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/apache2/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/apache2/html.mustache")
    Java    -> $(embedStringFile "embedded/license/apache2/java.mustache")
    JS      -> $(embedStringFile "embedded/license/apache2/js.mustache")
    Rust    -> $(embedStringFile "embedded/license/apache2/rust.mustache")
    Scala   -> $(embedStringFile "embedded/license/apache2/scala.mustache")
  BSD3 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/bsd3/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/bsd3/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/bsd3/html.mustache")
    Java    -> $(embedStringFile "embedded/license/bsd3/java.mustache")
    JS      -> $(embedStringFile "embedded/license/bsd3/js.mustache")
    Rust    -> $(embedStringFile "embedded/license/bsd3/rust.mustache")
    Scala   -> $(embedStringFile "embedded/license/bsd3/scala.mustache")
  GPL2 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/gpl2/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/gpl2/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/gpl2/html.mustache")
    Java    -> $(embedStringFile "embedded/license/gpl2/java.mustache")
    JS      -> $(embedStringFile "embedded/license/gpl2/js.mustache")
    Rust    -> $(embedStringFile "embedded/license/gpl2/rust.mustache")
    Scala   -> $(embedStringFile "embedded/license/gpl2/scala.mustache")
  GPL3 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/gpl3/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/gpl3/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/gpl3/html.mustache")
    Java    -> $(embedStringFile "embedded/license/gpl3/java.mustache")
    JS      -> $(embedStringFile "embedded/license/gpl3/js.mustache")
    Rust    -> $(embedStringFile "embedded/license/gpl3/rust.mustache")
    Scala   -> $(embedStringFile "embedded/license/gpl3/scala.mustache")
  MIT -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/mit/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/mit/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/mit/html.mustache")
    Java    -> $(embedStringFile "embedded/license/mit/java.mustache")
    JS      -> $(embedStringFile "embedded/license/mit/js.mustache")
    Rust    -> $(embedStringFile "embedded/license/mit/rust.mustache")
    Scala   -> $(embedStringFile "embedded/license/mit/scala.mustache")
  MPL2 -> case fileType of
    CSS     -> $(embedStringFile "embedded/license/mpl2/css.mustache")
    Haskell -> $(embedStringFile "embedded/license/mpl2/haskell.mustache")
    HTML    -> $(embedStringFile "embedded/license/mpl2/html.mustache")
    Java    -> $(embedStringFile "embedded/license/mpl2/java.mustache")
    JS      -> $(embedStringFile "embedded/license/mpl2/js.mustache")
    Rust    -> $(embedStringFile "embedded/license/mpl2/rust.mustache")
    Scala   -> $(embedStringFile "embedded/license/mpl2/scala.mustache")
