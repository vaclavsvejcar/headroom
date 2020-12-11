{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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
pattern for the configuration.
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
import           Headroom.Configuration.Compat  ( checkCompatibility )
import           Headroom.Configuration.Types   ( Configuration(..)
                                                , ConfigurationError(..)
                                                , ConfigurationKey(..)
                                                , CtConfiguration
                                                , CtHeaderConfig
                                                , CtHeaderFnConfig
                                                , CtHeaderFnConfigs
                                                , CtHeadersConfig
                                                , CtUpdateCopyrightConfig
                                                , HeaderConfig(..)
                                                , HeaderFnConfig(..)
                                                , HeaderFnConfigs(..)
                                                , HeadersConfig(..)
                                                , Phase(..)
                                                , PtConfiguration
                                                , PtHeaderConfig
                                                , PtHeaderFnConfig
                                                , PtHeaderFnConfigs
                                                , PtHeadersConfig
                                                , PtUpdateCopyrightConfig
                                                , UpdateCopyrightConfig(..)
                                                )
import           Headroom.Data.Lens             ( suffixLenses )
import           Headroom.FileType.Types        ( FileType(..) )
import           Headroom.Meta                  ( configBreakingChanges )
import           RIO
import qualified RIO.ByteString                as B


suffixLenses ''HeaderFnConfig
suffixLenses ''HeaderFnConfigs
suffixLenses ''UpdateCopyrightConfig


-- | Loads and parses application configuration from given /YAML/ file.
loadConfiguration :: (MonadIO m, MonadThrow m) => FilePath -> m PtConfiguration
loadConfiguration path = do
  content <- liftIO $ B.readFile path
  _       <- checkCompatibility configBreakingChanges content
  parseConfiguration content


-- | Parses application configuration from given raw input in /YAML/ format.
parseConfiguration :: MonadThrow m
                   => ByteString
                   -- ^ raw input to parse
                   -> m PtConfiguration
                   -- ^ parsed application configuration
parseConfiguration = Y.decodeThrow


-- | Makes full 'CtConfiguration' from provided 'PtConfiguration' (if valid).
makeConfiguration :: MonadThrow m
                  => PtConfiguration
                  -- ^ source 'PtConfiguration'
                  -> m CtConfiguration
                  -- ^ full 'CtConfiguration'
makeConfiguration pt = do
  cRunMode         <- lastOrError CkRunMode (cRunMode pt)
  cSourcePaths     <- lastOrError CkSourcePaths (cSourcePaths pt)
  cExcludedPaths   <- lastOrError CkExcludedPaths (cExcludedPaths pt)
  cTemplateSource  <- lastOrError CkTemplateSource (cTemplateSource pt)
  cLicenseHeaders  <- makeHeadersConfig (cLicenseHeaders pt)
  cHeaderFnConfigs <- makeHeaderFnConfigs (cHeaderFnConfigs pt)
  cVariables       <- pure $ cVariables pt
  pure Configuration { .. }


-- | Makes full 'CtHeadersConfig' from provided 'PtHeadersConfig' (if valid).
makeHeadersConfig :: MonadThrow m
                  => PtHeadersConfig
                  -- ^ source 'PtHeadersConfig'
                  -> m CtHeadersConfig
                  -- ^ full 'CtHeadersConfig'
makeHeadersConfig pt = do
  hscC          <- makeHeaderConfig C (hscC pt)
  hscCpp        <- makeHeaderConfig CPP (hscCpp pt)
  hscCss        <- makeHeaderConfig CSS (hscCss pt)
  hscHaskell    <- makeHeaderConfig Haskell (hscHaskell pt)
  hscHtml       <- makeHeaderConfig HTML (hscHtml pt)
  hscJava       <- makeHeaderConfig Java (hscJava pt)
  hscJs         <- makeHeaderConfig JS (hscJs pt)
  hscPureScript <- makeHeaderConfig PureScript (hscPureScript pt)
  hscRust       <- makeHeaderConfig Rust (hscRust pt)
  hscScala      <- makeHeaderConfig Scala (hscScala pt)
  hscShell      <- makeHeaderConfig Shell (hscShell pt)
  pure HeadersConfig { .. }


-- | Makes full 'CtHeaderConfig' from provided 'PtHeaderConfig' (if valid).
makeHeaderConfig :: MonadThrow m
                 => FileType
                 -- ^ determines for which file type this configuration is
                 -> PtHeaderConfig
                 -- ^ source 'PtHeaderConfig'
                 -> m CtHeaderConfig
                 -- ^ full 'CtHeaderConfig'
makeHeaderConfig fileType pt = do
  hcFileExtensions <- lastOrError (CkFileExtensions fileType)
                                  (hcFileExtensions pt)
  hcMarginTopCode <- lastOrError (CkMarginTopCode fileType) (hcMarginTopCode pt)
  hcMarginTopFile <- lastOrError (CkMarginTopFile fileType) (hcMarginTopFile pt)
  hcMarginBottomCode <- lastOrError (CkMarginBottomCode fileType)
                                    (hcMarginBottomCode pt)
  hcMarginBottomFile <- lastOrError (CkMarginBottomFile fileType)
                                    (hcMarginBottomFile pt)
  hcPutAfter     <- lastOrError (CkPutAfter fileType) (hcPutAfter pt)
  hcPutBefore    <- lastOrError (CkPutBefore fileType) (hcPutBefore pt)
  hcHeaderSyntax <- lastOrError (CkHeaderSyntax fileType) (hcHeaderSyntax pt)
  pure HeaderConfig { .. }


------------------------------  Private Functions  -----------------------------

makeHeaderFnConfigs :: MonadThrow m => PtHeaderFnConfigs -> m CtHeaderFnConfigs
makeHeaderFnConfigs pt = do
  hfcsUpdateCopyright <- makeHeaderFnConfig (pt ^. hfcsUpdateCopyrightL)
                                            makeUpdateCopyrightConfig
  pure HeaderFnConfigs { .. }


makeHeaderFnConfig :: MonadThrow m
                   => PtHeaderFnConfig c
                   -> (c 'Partial -> m (c 'Complete))
                   -> m (CtHeaderFnConfig c)
makeHeaderFnConfig pt fn = do
  hfcEnabled <- lastOrError CkEnabled (pt ^. hfcEnabledL)
  hfcConfig  <- fn $ pt ^. hfcConfigL
  pure HeaderFnConfig { .. }


makeUpdateCopyrightConfig :: MonadThrow m
                          => PtUpdateCopyrightConfig
                          -> m CtUpdateCopyrightConfig
makeUpdateCopyrightConfig pt = do
  let uccSelectedAuthors = lastOrNothing $ pt ^. uccSelectedAuthorsL
  pure UpdateCopyrightConfig { .. }


lastOrError :: MonadThrow m => ConfigurationKey -> Last a -> m a
lastOrError key (Last a) = maybe (throwM $ MissingConfiguration key) pure a


lastOrNothing :: Last (Maybe a) -> Maybe a
lastOrNothing (Last a) = fromMaybe Nothing a
