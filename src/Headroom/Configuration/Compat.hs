{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Configuration.Compat
Description : Compatibility checks for YAML configuration
Copyright   : (c) 2019-2020 Vaclav Svejcar
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

import           Data.Aeson                     ( FromJSON(..)
                                                , withObject
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y
import           Headroom.Meta                  ( buildVersion
                                                , configFileName
                                                , webDocMigration
                                                )
import           Headroom.Meta.Version          ( Version(..)
                                                , printVersion
                                                , pvp
                                                )
import           Headroom.Types                 ( fromHeadroomError
                                                , toHeadroomError
                                                )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


---------------------------------  DATA TYPES  ---------------------------------

data VersionWrapper = VersionWrapper
  { vVersion :: Version
  }
  deriving (Eq, Show)

instance FromJSON VersionWrapper where
  parseJSON = withObject "VersionWrapper" $ \obj -> do
    vVersion <- obj .: "version"
    pure VersionWrapper { .. }


---------------------------------  ERROR TYPES  --------------------------------

-- | Exception specific to the "Headroom.Configuration.Compat" module.
data VersionError
  = CannotParseVersion
  -- ^ cannot parse version info from given YAML configuration
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
                   -> ByteString
                   -- ^ raw, not yet parsed YAML configuration
                   -> m ()
                   -- ^ result of the operation
checkCompatibility configBreakingVersions raw = do
  VersionWrapper {..} <- parseWrapper
  case newerVersions configBreakingVersions vVersion of
    []       -> pure ()
    versions -> throwM $ UnsupportedVersion versions vVersion
 where
  newerVersions vs v = L.filter (v <) . L.sort $ vs
  parseWrapper = case Y.decodeEither' raw of
    Left  _       -> throwM CannotParseVersion
    Right version -> pure version


------------------------------  PRIVATE FUNCTIONS  -----------------------------

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
  UnsupportedVersion versions version ->
    mconcat
        [ "Your YAML configuration file ("
        , configFileName
        , ") has version 'v"
        , printVersion version
        , "' (set by 'version' field), which is incompatible with current "
        , "Headroom version 'v"
        , printVersion buildVersion
        , "'. Please do migration steps from following migration guides "
        , "(in same order):"
        ]
      <> mconcat steps
    where steps = fmap (\v -> "\n\t- " <> webDocMigration v) versions

