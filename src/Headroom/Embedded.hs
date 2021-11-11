{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Headroom.Embedded
Description : Embedded files
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Contains contents of files embedded using the "Data.FileEmbed" module.
-}

module Headroom.Embedded
  ( configFileStub
  , defaultConfig
  , defaultGlobalConfig
  , licenseTemplate
  )
where

import           Headroom.Config.Types               ( LicenseType(..) )
import           Headroom.Embedded.TH                ( embedConfigFile
                                                     , embedDefaultConfig
                                                     , embedDefaultGlobalConfig
                                                     , embedTemplate
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )
import           RIO


-- | Content of dummy /YAML/ configuration file for the application.
configFileStub :: IsString a => a
configFileStub = $(embedConfigFile)


-- | Default /YAML/ configuration.
defaultConfig :: IsString a => a
defaultConfig = $(embedDefaultConfig)


-- | Default /YAML/ configuration for the global configuration file.
defaultGlobalConfig :: IsString a => a
defaultGlobalConfig = $(embedDefaultGlobalConfig)


-- | License template for given license.
licenseTemplate :: IsString a
                => LicenseType
                -- ^ license for which to return the template
                -> FileType
                -- ^ license for which to return the template
                -> a
                -- ^ template text
licenseTemplate licenseType fileType = case licenseType of
  Apache2 -> case fileType of
    C          -> $(embedTemplate Apache2 C)
    CPP        -> $(embedTemplate Apache2 CPP)
    CSS        -> $(embedTemplate Apache2 CSS)
    Go         -> $(embedTemplate Apache2 Go)
    Haskell    -> $(embedTemplate Apache2 Haskell)
    HTML       -> $(embedTemplate Apache2 HTML)
    Java       -> $(embedTemplate Apache2 Java)
    JS         -> $(embedTemplate Apache2 JS)
    PHP        -> $(embedTemplate Apache2 PHP)
    PureScript -> $(embedTemplate Apache2 PureScript)
    Rust       -> $(embedTemplate Apache2 Rust)
    Scala      -> $(embedTemplate Apache2 Scala)
    Shell      -> $(embedTemplate Apache2 Shell)
  BSD3 -> case fileType of
    C          -> $(embedTemplate BSD3 C)
    CPP        -> $(embedTemplate BSD3 CPP)
    CSS        -> $(embedTemplate BSD3 CSS)
    Go         -> $(embedTemplate BSD3 Go)
    Haskell    -> $(embedTemplate BSD3 Haskell)
    HTML       -> $(embedTemplate BSD3 HTML)
    Java       -> $(embedTemplate BSD3 Java)
    JS         -> $(embedTemplate BSD3 JS)
    PHP        -> $(embedTemplate BSD3 PHP)
    PureScript -> $(embedTemplate BSD3 PureScript)
    Rust       -> $(embedTemplate BSD3 Rust)
    Scala      -> $(embedTemplate BSD3 Scala)
    Shell      -> $(embedTemplate BSD3 Shell)
  GPL2 -> case fileType of
    C          -> $(embedTemplate GPL2 C)
    CPP        -> $(embedTemplate GPL2 CPP)
    CSS        -> $(embedTemplate GPL2 CSS)
    Go         -> $(embedTemplate GPL2 Go)
    Haskell    -> $(embedTemplate GPL2 Haskell)
    HTML       -> $(embedTemplate GPL2 HTML)
    Java       -> $(embedTemplate GPL2 Java)
    JS         -> $(embedTemplate GPL2 JS)
    PHP        -> $(embedTemplate GPL2 PHP)
    PureScript -> $(embedTemplate GPL2 PureScript)
    Rust       -> $(embedTemplate GPL2 Rust)
    Scala      -> $(embedTemplate GPL2 Scala)
    Shell      -> $(embedTemplate GPL2 Shell)
  GPL3 -> case fileType of
    C          -> $(embedTemplate GPL3 C)
    CPP        -> $(embedTemplate GPL3 CPP)
    CSS        -> $(embedTemplate GPL3 CSS)
    Go         -> $(embedTemplate GPL3 Go)
    Haskell    -> $(embedTemplate GPL3 Haskell)
    HTML       -> $(embedTemplate GPL3 HTML)
    Java       -> $(embedTemplate GPL3 Java)
    JS         -> $(embedTemplate GPL3 JS)
    PHP        -> $(embedTemplate GPL3 PHP)
    PureScript -> $(embedTemplate GPL3 PureScript)
    Rust       -> $(embedTemplate GPL3 Rust)
    Scala      -> $(embedTemplate GPL3 Scala)
    Shell      -> $(embedTemplate GPL3 Shell)
  MIT -> case fileType of
    C          -> $(embedTemplate MIT C)
    CPP        -> $(embedTemplate MIT CPP)
    CSS        -> $(embedTemplate MIT CSS)
    Go         -> $(embedTemplate MIT Go)
    Haskell    -> $(embedTemplate MIT Haskell)
    HTML       -> $(embedTemplate MIT HTML)
    Java       -> $(embedTemplate MIT Java)
    JS         -> $(embedTemplate MIT JS)
    PHP        -> $(embedTemplate MIT PHP)
    PureScript -> $(embedTemplate MIT PureScript)
    Rust       -> $(embedTemplate MIT Rust)
    Scala      -> $(embedTemplate MIT Scala)
    Shell      -> $(embedTemplate MIT Shell)
  MPL2 -> case fileType of
    C          -> $(embedTemplate MPL2 C)
    CPP        -> $(embedTemplate MPL2 CPP)
    CSS        -> $(embedTemplate MPL2 CSS)
    Go         -> $(embedTemplate MPL2 Go)
    Haskell    -> $(embedTemplate MPL2 Haskell)
    HTML       -> $(embedTemplate MPL2 HTML)
    Java       -> $(embedTemplate MPL2 Java)
    JS         -> $(embedTemplate MPL2 JS)
    PHP        -> $(embedTemplate MPL2 PHP)
    PureScript -> $(embedTemplate MPL2 PureScript)
    Rust       -> $(embedTemplate MPL2 Rust)
    Scala      -> $(embedTemplate MPL2 Scala)
    Shell      -> $(embedTemplate MPL2 Shell)
