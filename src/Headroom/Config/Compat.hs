{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Config.Compat
Description : Compatibility checks for YAML configuration
Copyright   : (c) 2019-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions and data types used for checking compatibility of
user's YAML configuration with current version of Headroom.
-}

module Headroom.Config.Compat
  ( VersionError(..)
  , checkCompatibility
  )
where

import           Data.Aeson                          ( FromJSON(..)
                                                     , withObject
                                                     , (.:)
                                                     )
import           Data.String.Interpolate             ( iii )
import qualified Data.Yaml                          as Y
import           Headroom.Meta                       ( buildVersion
                                                     , configFileName
                                                     , productName
                                                     , webDocMigration
                                                     )
import           Headroom.Meta.Version               ( Version(..)
                                                     , printVersionP
                                                     , pvp
                                                     )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           RIO
import qualified RIO.List                           as L


---------------------------------  DATA TYPES  ---------------------------------

newtype VersionObj = VersionObj Version deriving (Eq, Show)

instance FromJSON VersionObj where
  parseJSON = withObject "VersionObj" $ \obj -> do
    version <- obj .: "version"
    pure $ VersionObj version


---------------------------------  ERROR TYPES  --------------------------------

-- | Exception specific to the "Headroom.Configuration.Compat" module.
data VersionError
  = CannotParseVersion                   -- ^ cannot parse version info from YAML
  | CannotParseYaml Y.ParseException     -- ^ error parsing YAML file
  | NewerVersionDetected Version         -- ^ configuration has too new version
  | UnsupportedVersion [Version] Version -- ^ given YAML configuration is not compatible
  deriving (Show)


instance Exception VersionError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Checks whether the given not yet parsed YAML configuration is compatible,
-- using list of versions that caused breaking changes into configuration.
checkCompatibility :: MonadThrow m
                   => [Version]
                   -- ^ list of versions with breaking changes in configuration
                   -> Version
                   -- ^ current Headroom version
                   -> ByteString
                   -- ^ raw, not yet parsed YAML configuration
                   -> m Version
                   -- ^ detected compatible version or error
checkCompatibility breakingVersions current raw = do
  VersionObj version <- parseObj
  _                  <- checkBreakingChanges breakingVersions version
  _                  <- checkNewerVersion current version
  pure version
 where
  parseObj = either (throwM . handleEx) pure decoded
  decoded  = Y.decodeEither' raw
  handleEx = \case
    err@(Y.InvalidYaml _) -> CannotParseYaml err
    _                     -> CannotParseVersion


------------------------------  PRIVATE FUNCTIONS  -----------------------------

checkBreakingChanges :: MonadThrow m => [Version] -> Version -> m ()
checkBreakingChanges vs v = case L.filter (v <) . L.sort $ vs of
  []    -> pure ()
  newer -> throwM $ UnsupportedVersion newer v


checkNewerVersion :: MonadThrow m => Version -> Version -> m ()
checkNewerVersion current checked =
  if current < checked then throwM $ NewerVersionDetected checked else pure ()


displayException' :: VersionError -> String
displayException' = \case
  CannotParseVersion -> [iii|
      Cannot find 'version' key in #{configFileName :: String} configuration
      file. This field is required to check whether your current configuration
      is compatible with installed version of #{productName}. This functionality
      has been added in version 0.4.0.0, please see following migration guide
      for more details on how to proceed:
      #{"\n\t" <> webDocMigration v0400}
    |]
  CannotParseYaml ex -> [iii|
      Cannot parse #{configFileName :: String} configuration file:
      #{"\n" <> Y.prettyPrintParseException ex}
    |]
  NewerVersionDetected version -> [iii|
      The version set in your #{configFileName :: String} configuration file
      (#{printVersionP version}) is newer than version of installed
      #{productName} (#{printVersionP buildVersion}). Please upgrade
      #{productName} first.
    |]
  UnsupportedVersion versions version -> [iii|
      Your #{configFileName :: String} configuration file has version
      #{printVersionP version}, which is incompatible with current version of
      #{productName} (#{printVersionP buildVersion}). Please perform steps
      described in these migration guides first (in given order):
      #{migrationGuides versions}
    |]
 where
  v0400           = [pvp|0.4.0.0|]
  migrationGuides = mconcat . fmap (\v -> "\n\t- " <> webDocMigration v)
