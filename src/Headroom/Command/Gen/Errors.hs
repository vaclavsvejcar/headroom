{-|
Module      : Headroom.Command.Gen.Errors
Description : Gen command error data types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types representing errors occuring during /Generator/ command.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Command.Gen.Errors
  ( GenCommandError(..)
  , genCommandErrorMessage
  )
where

import           Headroom.FileType              ( FileType )
import           Headroom.License               ( LicenseType )
import           Headroom.Types.Utils           ( showEnumValuesLC )
import           RIO
import qualified RIO.Text                      as T



-- | Errors specific for the /Gen/ command.
data GenCommandError
  = InvalidLicense Text -- ^ invalid license entered by the user
  | NoGenModeSelected   -- ^ no mode of /Gen/ command selected
  deriving Show

-- | User-friendly description of the given error.
genCommandErrorMessage :: GenCommandError -- ^ error to get message for
                       -> Text            -- ^ error message
genCommandErrorMessage = \case
  InvalidLicense tried -> invalidLicense tried
  NoGenModeSelected    -> noGenModeSelected

invalidLicense :: Text -> Text
invalidLicense tried = mconcat
  [ "Invalid license type '"
  , tried
  , "', must be in format licenseType:fileType (e.g. bsd3:haskell). "
  , "\nAvailable license types: "
  , T.pack $ showEnumValuesLC @LicenseType
  , ".\nAvailable file types: "
  , T.pack $ showEnumValuesLC @FileType
  ]

noGenModeSelected :: Text
noGenModeSelected = mconcat
  [ "Please select at least one option what to generate "
  , "(see --help for details)"
  ]
