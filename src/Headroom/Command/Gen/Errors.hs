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
module Headroom.Command.Gen.Errors
  ( GenCommandError(..)
  , genCommandErrorMessage
  )
where

import           RIO


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
invalidLicense tried = "Cannot parse license type from: " <> tried

noGenModeSelected :: Text
noGenModeSelected = mconcat
  [ "Please select at least one option what to generate "
  , "(see --help for details)"
  ]
