{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Headroom.Configuration
  ( defaultPartialConfiguration
  , defaultPartialHeadersConfig
  , makeConfiguration
  )
where

import           Data.Monoid                    ( Last(..) )
import           Headroom.Header                ( haskellHeaderConfig
                                                , htmlHeaderConfig
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
                                                , RunMode(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM


defaultPartialConfiguration :: PartialConfiguration
defaultPartialConfiguration = mempty
  { pcRunMode        = pure Add
  , pcVariables      = pure $ HM.fromList []
  , pcLicenseHeaders = defaultPartialHeadersConfig
  }

defaultPartialHeadersConfig :: PartialHeadersConfig
defaultPartialHeadersConfig =
  mempty { phscHaskell = haskellHeaderConfig, phscHTML = htmlHeaderConfig }

makeConfiguration :: MonadThrow m => PartialConfiguration -> m Configuration
makeConfiguration PartialConfiguration {..} = do
  cRunMode        <- lastOrError NoRunMode pcRunMode
  cSourcePaths    <- lastOrError NoSourcePaths pcSourcePaths
  cTemplatePaths  <- lastOrError NoTemplatePaths pcTemplatePaths
  cVariables      <- lastOrError NoVariables pcVariables
  cLicenseHeaders <- makeHeadersConfig pcLicenseHeaders
  pure Configuration { .. }

makeHeadersConfig :: MonadThrow m => PartialHeadersConfig -> m HeadersConfig
makeHeadersConfig PartialHeadersConfig {..} = do
  hscHaskell <- makeHeaderConfig Haskell phscHaskell
  hscHtml    <- makeHeaderConfig HTML phscHTML
  pure HeadersConfig { .. }

makeHeaderConfig :: MonadThrow m
                 => FileType
                 -> PartialHeaderConfig
                 -> m HeaderConfig
makeHeaderConfig fileType PartialHeaderConfig {..} = do
  hcFileExtensions <- lastOrError (NoFileExtensions fileType) phcFileExtensions
  hcPutAfter       <- lastOrError (NoPutAfter fileType) phcPutAfter
  hcStartsWith     <- lastOrError (NoStartsWith fileType) phcStartsWith
  hcEndsWith       <- lastOrError (NoEndsWith fileType) phcEndsWith
  pure HeaderConfig { .. }

lastOrError :: MonadThrow m => ConfigurationError -> Last a -> m a
lastOrError err (Last x) = maybe (throwM $ ConfigurationError err) pure x

