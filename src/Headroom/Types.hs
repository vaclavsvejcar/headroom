{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Types
  ( PartialConfiguration(..)
  , Configuration(..)
  , ApplicationError(..)
  , CommandGenError(..)
  , CommandInitError(..)
  , CommandInitOptions(..)
  , ConfigurationError(..)
  , TemplateError(..)
  , RunMode(..)
  , GenMode(..)
  , Command(..)
  , CommandGenOptions(..)
  , LicenseType(..)
  , FileType(..)
  )
where

import           Data.Aeson                     ( ToJSON(..)
                                                , genericToJSON
                                                )
import           Data.Monoid                    ( Last )
import           Headroom.Serialization         ( aesonOptions )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
import           RIO


data RunMode
  = Add
  | Drop
  | Replace
  deriving (Eq, Show)

instance ToJSON RunMode where
  toJSON = \case
    Add     -> "add"
    Drop    -> "drop"
    Replace -> "replace"

-- | Represents what action should the /Generator/ perform.
data GenMode
  = GenConfigFile   -- ^ generate /YAML/ config file stub
  | GenLicense (LicenseType, FileType) -- ^ generate license header template
  deriving (Eq, Show)

data ApplicationError
  = CommandGenError CommandGenError
  | CommandInitError CommandInitError
  | ConfigurationError ConfigurationError
  | TemplateError TemplateError
  deriving (Eq, Show)

instance Exception ApplicationError

-- | Errors specific for the /Gen/ command.
data CommandGenError
  = InvalidLicenseOrFileType Text -- ^ invalid license entered by the user
  | NoGenModeSelected             -- ^ no mode of /Gen/ command selected
  deriving (Eq, Show)

data CommandInitError
  = AppConfigAlreadyExists FilePath -- ^ application configuration file already exists
  | InvalidLicenseType Text         -- ^ invalid license type specified
  | NoProvidedSourcePaths           -- ^ no paths to source code files provided
  | NoSupportedFileType             -- ^ no supported file types found on source paths
  deriving (Eq, Show)

data ConfigurationError
  = NoRunMode
  | NoSourcePaths
  | NoTemplatePaths
  | NoVariables
  deriving (Eq, Show)

data TemplateError
  = MissingVariables Text [Text]
  | ParseError Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Application command.
data Command
  = Run [FilePath] [FilePath] [Text] RunMode Bool -- ^ /Run/ command
  | Gen Bool (Maybe (LicenseType, FileType))                         -- ^ /Generator/ command
  | Init LicenseType [FilePath]                          -- ^ /Init/ command
  deriving (Show)

--------------------------------------------------------------------------------

newtype CommandGenOptions = CommandGenOptions
  { cgoGenMode :: GenMode
  }
  deriving (Show)

-- | Options for the /Init/ command.
data CommandInitOptions = CommandInitOptions
  { cioSourcePaths :: ![FilePath]
  , cioLicenseType :: !LicenseType
  }
  deriving Show

--------------------------------------------------------------------------------

data FileType
  = CSS
  | Haskell
  | HTML
  | Java
  | JS
  | Scala
  deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)

--------------------------------------------------------------------------------

data LicenseType
  = Apache2
  | BSD3
  | GPL2
  | GPL3
  | MIT
  | MPL2
  deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)

--------------------------------------------------------------------------------

data Configuration = Configuration
  { cRunMode       :: RunMode
  , cSourcePaths   :: [FilePath]
  , cTemplatePaths :: [FilePath]
  , cVariables     :: HashMap Text Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON Configuration where
  toJSON = genericToJSON aesonOptions


--------------------------------------------------------------------------------

data PartialConfiguration = PartialConfiguration
  { pcRunMode       :: Last RunMode
  , pcSourcePaths   :: Last [FilePath]
  , pcTemplatePaths :: Last [FilePath]
  , pcVariables     :: Last (HashMap Text Text)
  }
  deriving (Eq, Show)

instance Semigroup PartialConfiguration where
  x <> y = PartialConfiguration
    { pcRunMode       = pcRunMode x <> pcRunMode y
    , pcSourcePaths   = pcSourcePaths x <> pcSourcePaths y
    , pcTemplatePaths = pcTemplatePaths x <> pcTemplatePaths y
    , pcVariables     = pcVariables x <> pcVariables y
    }

instance Monoid PartialConfiguration where
  mempty = PartialConfiguration mempty mempty mempty mempty

--------------------------------------------------------------------------------
