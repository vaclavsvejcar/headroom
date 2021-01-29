{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Configuration.Compat
Description : Compatibility checks for YAML configuration
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions and data types used for checking compatibility of
user's YAML configuration with current version of Headroom.
-}

module Headroom.Configuration.Compat
  ( VersionError(..)
  , checkCompatibility
  )
where

import           Data.Aeson                          ( FromJSON(..)
                                                     , withObject
                                                     , (.:)
                                                     )
import qualified Data.Yaml                          as Y
import           Headroom.Meta                       ( buildVersion
                                                     , configFileName
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
import qualified RIO.Text                           as T


---------------------------------  DATA TYPES  ---------------------------------

newtype VersionObj = VersionObj Version deriving (Eq, Show)

instance FromJSON VersionObj where
  parseJSON = withObject "VersionObj" $ \obj -> do
    version <- obj .: "version"
    pure $ VersionObj version


---------------------------------  ERROR TYPES  --------------------------------

-- | Exception specific to the "Headroom.Configuration.Compat" module.
data VersionError
  = CannotParseVersion
  -- ^ cannot parse version info from given YAML configuration
  | NewerVersionDetected Version
  -- ^ configuration has newer version than Headroom
  | UnsupportedVersion [Version] Version
  -- ^ given YAML configuration is not compatible
  deriving (Eq, Show)


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
  parseObj = either (const . throwM $ CannotParseVersion) pure decoded
  decoded  = Y.decodeEither' raw


------------------------------  PRIVATE FUNCTIONS  -----------------------------

checkBreakingChanges :: MonadThrow m => [Version] -> Version -> m ()
checkBreakingChanges vs v = case L.filter (v <) . L.sort $ vs of
  []    -> pure ()
  newer -> throwM $ UnsupportedVersion newer v


checkNewerVersion :: MonadThrow m => Version -> Version -> m ()
checkNewerVersion current checked =
  if current < checked then throwM $ NewerVersionDetected checked else pure ()


displayException' :: VersionError -> String
displayException' = T.unpack . \case
  CannotParseVersion -> mconcat
    [ "Cannot find 'version' key in your YAML configuration file ("
    , configFileName
    , "), which is "
    , "required to check compatibility of configuration file. This "
    , "functionality has been added in version 'v0.4.0.0', please check "
    , "following migration guide for more details:\n"
    , "\t- "
    , webDocMigration [pvp|0.4.0.0|]
    ]
  NewerVersionDetected version -> mconcat
    [ "Your YAML configuration file ("
    , configFileName
    , ") has newer version '"
    , printVersionP version
    , "' than your current Headroom version '"
    , printVersionP buildVersion
    , "'. Please upgrade Headroom first."
    ]
  UnsupportedVersion versions version ->
    mconcat
        [ "Your YAML configuration file ("
        , configFileName
        , ") has version '"
        , printVersionP version
        , "' (set by 'version' field), which is incompatible with current "
        , "Headroom version '"
        , printVersionP buildVersion
        , "'. Please do migration steps from following migration guides "
        , "(in same order):"
        ]
      <> mconcat steps
    where steps = fmap (\v -> "\n\t- " <> webDocMigration v) versions

