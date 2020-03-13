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
  ( AppConfigError(..)
  , HeadroomError(..)
  , NewLine(..)
  , Progress(..)
  , RunMode(..)
  , InitCommandError(..)
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(String)
                                                )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Printf                    ( printf )

-- | Error occured during validation of application configuration.
data AppConfigError
  = EmptySourcePaths       -- ^ no paths to source code files provided
  | EmptyTemplatePaths     -- ^ no paths to license header templates provided
  deriving (Show)

-- | Represents fatal application error, that should be displayed to user in
-- some human readable form.
data HeadroomError
  = InvalidAppConfig [AppConfigError] -- ^ invalid application configuration
  | InvalidLicense Text               -- ^ unknown license is selected in /Generator/
  | InvalidVariable Text              -- ^ invalid variable format (@key=value@)
  | MissingVariables Text [Text]      -- ^ not all variables were filled in template
  | NoGenModeSelected                 -- ^ no mode for /Generator/ command is selected
  | ParseError Text                   -- ^ error parsing template file
  | InitCommandError InitCommandError
  deriving (Show, Typeable)

data InitCommandError
  = AppConfigAlreadyExists
  | NoSourcePaths
  | NoSupportedFileType
  deriving (Show)

-- | Represents newline separator.
data NewLine
  = CR   -- ^ line ends with @\r@
  | CRLF -- ^ line ends with @\r\n@
  | LF   -- ^ line ends with @\n@
  deriving (Eq, Show)

-- | Progress indication. First argument is current progress, second the maximum
-- value.
data Progress = Progress Int Int
  deriving Eq

-- | Mode of the /Run/ command, states how to license headers in source code
-- files.
data RunMode
  = Add     -- ^ add license header if missing in source code file
  | Drop    -- ^ drop any license header if present in source code file
  | Replace -- ^ replace existing or add license header
  deriving (Eq, Show)

displayAppConfigError :: AppConfigError -> Text
displayAppConfigError = \case
  EmptySourcePaths   -> "no paths to source code files"
  EmptyTemplatePaths -> "no paths to template files"

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance Exception HeadroomError where
  displayException = \case
    InvalidAppConfig errors -> mconcat
      [ "Invalid configuration, following problems found:\n"
      , L.intercalate
        "\n"
        (fmap (\e -> "\t- " <> (T.unpack . displayAppConfigError $ e)) errors)
      ]
    InvalidLicense raw -> "Cannot parse license type from: " <> T.unpack raw
    InvalidVariable raw ->
      "Cannot parse variable key=value from: " <> T.unpack raw
    NoGenModeSelected
      -> "Please select at least one option what to generate (see --help for details)"
    MissingVariables name variables -> mconcat
      ["Missing variables for template '", T.unpack name, "': ", show variables]
    ParseError       msg     -> "Error parsing template: " <> T.unpack msg
    InitCommandError icError -> case icError of
      AppConfigAlreadyExists -> "Config file '.headroom.yaml' already exists"
      NoSourcePaths          -> "No path to source code files defined"
      NoSupportedFileType    -> "No supported file type found"

instance Show Progress where
  show (Progress current total) = mconcat ["[", currentS, " of ", totalS, "]"]
   where
    format   = "%" <> (show . L.length $ totalS) <> "d"
    currentS = printf format current
    totalS   = show total

instance FromJSON RunMode where
  parseJSON (String s) = case T.toLower s of
    "add"     -> pure Add
    "drop"    -> pure Drop
    "replace" -> pure Replace
    _         -> error $ "Unknown run mode: " <> T.unpack s
  parseJSON other = error $ "Invalid value for run mode: " <> show other
