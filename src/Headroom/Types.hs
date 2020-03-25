{-|
Module      : Headroom.Types
Description : Data types and instances
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and type class instances shared between modules.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Types
  ( HeadroomError(..)
  , NewLine(..)
  , RunMode(..)
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                )
import           Headroom.AppConfig.Errors      ( AppConfigError(..)
                                                , appConfigErrorMessage
                                                )
import           Headroom.Command.Gen.Errors    ( GenCommandError(..)
                                                , genCommandErrorMessage
                                                )
import           Headroom.Command.Init.Errors   ( InitCommandError(..)
                                                , initCommandErrorMessage
                                                )
import           Headroom.Template              ( TemplateError
                                                , templateErrorMessage
                                                )
import           RIO
import qualified RIO.Text                      as T


-- | Represents fatal application error, that should be displayed to user in
-- some human readable form.
data HeadroomError
  = AppConfigError AppConfigError     -- ^ error when processing application config
  | GenCommandError GenCommandError   -- ^ error during execution of /Gen/ command
  | InitCommandError InitCommandError -- ^ error during execution of /Init/ command
  | TemplateError TemplateError       -- ^ error processing template
  deriving (Show, Typeable)

-- | Represents newline separator.
data NewLine
  = CR   -- ^ line ends with @\r@
  | CRLF -- ^ line ends with @\r\n@
  | LF   -- ^ line ends with @\n@
  deriving (Eq, Show)

-- | Mode of the /Run/ command, states how to license headers in source code
-- files.
data RunMode
  = Add     -- ^ add license header if missing in source code file
  | Drop    -- ^ drop any license header if present in source code file
  | Replace -- ^ replace existing or add license header
  deriving (Eq, Show)

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance Exception HeadroomError where
  displayException = \case
    AppConfigError   error' -> T.unpack $ appConfigErrorMessage error'
    GenCommandError  error' -> T.unpack $ genCommandErrorMessage error'
    InitCommandError error' -> T.unpack $ initCommandErrorMessage error'
    TemplateError    error' -> T.unpack $ templateErrorMessage error'

instance FromJSON RunMode where
  parseJSON (String s) = case T.toLower s of
    "add"     -> pure Add
    "drop"    -> pure Drop
    "replace" -> pure Replace
    _         -> error $ "Unknown run mode: " <> T.unpack s
  parseJSON other = error $ "Invalid value for run mode: " <> show other

instance ToJSON RunMode where
  toJSON = \case
    Add     -> "add"
    Drop    -> "drop"
    Replace -> "replace"
