{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Headroom.Embedded
Description : Embedded files
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Contains contents of files embedded using the "Data.FileEmbed" module.
-}

module Headroom.Embedded
  ( configFileStub
  , defaultConfig
  , licenseTemplate
  )
where

import           Headroom.Configuration.Types   ( LicenseType(..) )
import           Headroom.Embedded.TH           ( embedConfigFile
                                                , embedDefaultConfig
                                                , embedTemplate
                                                )
import           Headroom.FileType.Types        ( FileType(..) )
import           RIO


-- | Content of dummy /YAML/ configuration file for the application.
configFileStub :: IsString a => a
configFileStub = $(embedConfigFile)


-- | Default /YAML/ configuration.
defaultConfig :: IsString a => a
defaultConfig = $(embedDefaultConfig)


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
    Haskell    -> $(embedTemplate Apache2 Haskell)
    HTML       -> $(embedTemplate Apache2 HTML)
    Java       -> $(embedTemplate Apache2 Java)
    JS         -> $(embedTemplate Apache2 JS)
    PureScript -> $(embedTemplate Apache2 PureScript)
    Rust       -> $(embedTemplate Apache2 Rust)
    Scala      -> $(embedTemplate Apache2 Scala)
    Shell      -> $(embedTemplate Apache2 Shell)
  BSD3 -> case fileType of
    C          -> $(embedTemplate BSD3 C)
    CPP        -> $(embedTemplate BSD3 CPP)
    CSS        -> $(embedTemplate BSD3 CSS)
    Haskell    -> $(embedTemplate BSD3 Haskell)
    HTML       -> $(embedTemplate BSD3 HTML)
    Java       -> $(embedTemplate BSD3 Java)
    JS         -> $(embedTemplate BSD3 JS)
    PureScript -> $(embedTemplate BSD3 PureScript)
    Rust       -> $(embedTemplate BSD3 Rust)
    Scala      -> $(embedTemplate BSD3 Scala)
    Shell      -> $(embedTemplate BSD3 Shell)
  GPL2 -> case fileType of
    C          -> $(embedTemplate GPL2 C)
    CPP        -> $(embedTemplate GPL2 CPP)
    CSS        -> $(embedTemplate GPL2 CSS)
    Haskell    -> $(embedTemplate GPL2 Haskell)
    HTML       -> $(embedTemplate GPL2 HTML)
    Java       -> $(embedTemplate GPL2 Java)
    JS         -> $(embedTemplate GPL2 JS)
    PureScript -> $(embedTemplate GPL2 PureScript)
    Rust       -> $(embedTemplate GPL2 Rust)
    Scala      -> $(embedTemplate GPL2 Scala)
    Shell      -> $(embedTemplate GPL2 Shell)
  GPL3 -> case fileType of
    C          -> $(embedTemplate GPL3 C)
    CPP        -> $(embedTemplate GPL3 CPP)
    CSS        -> $(embedTemplate GPL3 CSS)
    Haskell    -> $(embedTemplate GPL3 Haskell)
    HTML       -> $(embedTemplate GPL3 HTML)
    Java       -> $(embedTemplate GPL3 Java)
    JS         -> $(embedTemplate GPL3 JS)
    PureScript -> $(embedTemplate GPL3 PureScript)
    Rust       -> $(embedTemplate GPL3 Rust)
    Scala      -> $(embedTemplate GPL3 Scala)
    Shell      -> $(embedTemplate GPL3 Shell)
  MIT -> case fileType of
    C          -> $(embedTemplate MIT C)
    CPP        -> $(embedTemplate MIT CPP)
    CSS        -> $(embedTemplate MIT CSS)
    Haskell    -> $(embedTemplate MIT Haskell)
    HTML       -> $(embedTemplate MIT HTML)
    Java       -> $(embedTemplate MIT Java)
    JS         -> $(embedTemplate MIT JS)
    PureScript -> $(embedTemplate MIT PureScript)
    Rust       -> $(embedTemplate MIT Rust)
    Scala      -> $(embedTemplate MIT Scala)
    Shell      -> $(embedTemplate MIT Shell)
  MPL2 -> case fileType of
    C          -> $(embedTemplate MPL2 C)
    CPP        -> $(embedTemplate MPL2 CPP)
    CSS        -> $(embedTemplate MPL2 CSS)
    Haskell    -> $(embedTemplate MPL2 Haskell)
    HTML       -> $(embedTemplate MPL2 HTML)
    Java       -> $(embedTemplate MPL2 Java)
    JS         -> $(embedTemplate MPL2 JS)
    PureScript -> $(embedTemplate MPL2 PureScript)
    Rust       -> $(embedTemplate MPL2 Rust)
    Scala      -> $(embedTemplate MPL2 Scala)
    Shell      -> $(embedTemplate MPL2 Shell)
