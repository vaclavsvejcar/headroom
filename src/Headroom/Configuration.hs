{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Headroom.Configuration
Description : Configuration handling (loading, parsing, validating)
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides logic for working with the cofiguration data types.
Headroom uses the
<https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67 partial options monoid>
pattern for the configuration, where the 'Configuration' is the data type for
total configuration and 'PartialConfiguration' for the partial one.
-}

module Headroom.Configuration
  ( -- * Loading & Parsing Configuration
    loadConfiguration
  , parseConfiguration
    -- * Processing Partial Configuration
  , makeConfiguration
  , makeHeadersConfig
  , makeHeaderConfig
  )
where

import           Data.Monoid                    ( Last(..) )
import qualified Data.Yaml                     as Y
import           Headroom.Configuration.Types   ( HeaderFnConfig
                                                , HeaderFnConfig'(..)
                                                , HeaderFnConfigs
                                                , HeaderFnConfigs'(..)
                                                , Phase(..)
                                                , PtHeaderFnConfig
                                                , PtHeaderFnConfigs
                                                , PtUpdateCopyrightConfig
                                                , UpdateCopyrightConfig
                                                , UpdateCopyrightConfig'(..)
                                                , hfcConfigL
                                                , hfcEnabledL
                                                , hfcsUpdateCopyrightL
                                                , hfcsUpdateCopyrightL
                                                , uccSelectedAuthorsL
                                                )
import           Headroom.Types                 ( ApplicationError(..)
                                                , Configuration(..)
                                                , ConfigurationError(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                , HeadersConfig(..)
                                                , PartialConfiguration(..)
                                                , PartialHeaderConfig(..)
                                                , PartialHeadersConfig(..)
                                                )
import           RIO
import qualified RIO.ByteString                as B


-- | Loads and parses application configuration from given /YAML/ file.
loadConfiguration :: MonadIO m
                  => FilePath
                  -- ^ path to /YAML/ configuration file
                  -> m PartialConfiguration
                  -- ^ parsed configuration
loadConfiguration path = liftIO $ B.readFile path >>= parseConfiguration


-- | Parses application configuration from given raw input in /YAML/ format.
parseConfiguration :: MonadThrow m
                   => B.ByteString
                   -- ^ raw input to parse
                   -> m PartialConfiguration
                   -- ^ parsed application configuration
parseConfiguration = Y.decodeThrow


-- | Makes full 'Configuration' from provided 'PartialConfiguration' (if valid).
makeConfiguration :: MonadThrow m
                  => PartialConfiguration
                  -- ^ source 'PartialConfiguration'
                  -> m Configuration
                  -- ^ full 'Configuration'
makeConfiguration PartialConfiguration {..} = do
  cRunMode         <- lastOrError NoRunMode pcRunMode
  cSourcePaths     <- lastOrError NoSourcePaths pcSourcePaths
  cExcludedPaths   <- lastOrError NoExcludedPaths pcExcludedPaths
  cTemplateSource  <- lastOrError NoTemplateSource pcTemplateSource
  cLicenseHeaders  <- makeHeadersConfig pcLicenseHeaders
  cHeaderFnConfigs <- makeHeaderFnConfigs pcHeaderFnConfigs
  let cVariables = pcVariables
  pure Configuration { .. }


-- | Makes full 'HeadersConfig' from provided 'PartialHeadersConfig' (if valid).
makeHeadersConfig :: MonadThrow m
                  => PartialHeadersConfig
                  -- ^ source 'PartialHeadersConfig'
                  -> m HeadersConfig
                  -- ^ full 'HeadersConfig'
makeHeadersConfig PartialHeadersConfig {..} = do
  hscC       <- makeHeaderConfig C phscC
  hscCpp     <- makeHeaderConfig CPP phscCpp
  hscCss     <- makeHeaderConfig CSS phscCss
  hscHaskell <- makeHeaderConfig Haskell phscHaskell
  hscHtml    <- makeHeaderConfig HTML phscHtml
  hscJava    <- makeHeaderConfig Java phscJava
  hscJs      <- makeHeaderConfig JS phscJs
  hscRust    <- makeHeaderConfig Rust phscRust
  hscScala   <- makeHeaderConfig Scala phscScala
  hscShell   <- makeHeaderConfig Shell phscShell
  pure HeadersConfig { .. }


-- | Makes full 'HeaderConfig' from provided 'PartialHeaderConfig' (if valid).
makeHeaderConfig :: MonadThrow m
                 => FileType
                 -- ^ determines for which file type this configuration is
                 -> PartialHeaderConfig
                 -- ^ source 'PartialHeaderConfig'
                 -> m HeaderConfig
                 -- ^ full 'HeaderConfig'
makeHeaderConfig fileType PartialHeaderConfig {..} = do
  hcFileExtensions <- lastOrError (NoFileExtensions fileType) phcFileExtensions
  hcMarginAfter    <- lastOrError (NoMarginAfter fileType) phcMarginAfter
  hcMarginBefore   <- lastOrError (NoMarginBefore fileType) phcMarginBefore
  hcPutAfter       <- lastOrError (NoPutAfter fileType) phcPutAfter
  hcPutBefore      <- lastOrError (NoPutBefore fileType) phcPutBefore
  hcHeaderSyntax   <- lastOrError (NoHeaderSyntax fileType) phcHeaderSyntax
  pure HeaderConfig { .. }


makeHeaderFnConfigs :: MonadThrow m => PtHeaderFnConfigs -> m HeaderFnConfigs
makeHeaderFnConfigs pt = do
  hfcsUpdateCopyright <- makeHeaderFnConfig (pt ^. hfcsUpdateCopyrightL)
                                            makeUpdateCopyrightConfig
  pure HeaderFnConfigs { .. }


makeHeaderFnConfig :: MonadThrow m
                   => PtHeaderFnConfig c
                   -> (c 'Partial -> m (c 'Complete))
                   -> m (HeaderFnConfig c)
makeHeaderFnConfig pt fn = do
  hfcEnabled <- lastOrError NoEnabled (pt ^. hfcEnabledL)
  hfcConfig  <- fn $ pt ^. hfcConfigL
  pure HeaderFnConfig { .. }


makeUpdateCopyrightConfig :: MonadThrow m
                          => PtUpdateCopyrightConfig
                          -> m UpdateCopyrightConfig
makeUpdateCopyrightConfig pt = do
  let uccSelectedAuthors = lastOrNothing $ pt ^. uccSelectedAuthorsL
  pure UpdateCopyrightConfig { .. }


------------------------------  Private Functions  -----------------------------


lastOrError :: MonadThrow m => ConfigurationError -> Last a -> m a
lastOrError err (Last a) = maybe (throwM $ ConfigurationError err) pure a


lastOrNothing :: Last (Maybe a) -> Maybe a
lastOrNothing (Last a) = fromMaybe Nothing a
