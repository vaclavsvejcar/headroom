{-|
Module      : Headroom.Command.Init.Errors
Description : Init command error data types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types representing errors occuring during /Init/ command.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Command.Init.Errors
  ( InitCommandError(..)
  , initCommandErrorMessage
  )
where

import           Headroom.License               ( LicenseType )
import           Headroom.Types.Utils           ( allValues )
import           RIO
import qualified RIO.Text                      as T

-- | Errors specific for the /Init/ command.
data InitCommandError
  = AppConfigAlreadyExists FilePath -- ^ application configuration file already exists
  | InvalidLicenseType Text         -- ^ invalid license type specified
  | NoSourcePaths                   -- ^ no paths to source code files provided
  | NoSupportedFileType             -- ^ no supported file types found on source paths
  deriving (Show)

-- | User-friendly description of the given error.
initCommandErrorMessage :: InitCommandError -- ^ error to get message for
                        -> Text             -- ^ error message
initCommandErrorMessage = \case
  AppConfigAlreadyExists path  -> appConfigAlreadyExists path
  InvalidLicenseType     tried -> invalidLicenseType tried
  NoSourcePaths                -> noSourcePaths
  NoSupportedFileType          -> noSupportedFileType

appConfigAlreadyExists :: FilePath -> Text
appConfigAlreadyExists path =
  mconcat ["Configuration file '", T.pack path, "' already exists"]

invalidLicenseType :: Text -> Text
invalidLicenseType tried = mconcat
  [ "Invalid license type '"
  , tried
  , "', available options: "
  , T.pack . show $ available
  ]
  where available = allValues :: [LicenseType]

noSourcePaths :: Text
noSourcePaths = "No source code paths (files or directories) defined"

noSupportedFileType :: Text
noSupportedFileType = "No supported file type found in scanned source paths"
