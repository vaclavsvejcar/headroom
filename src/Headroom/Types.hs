{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Headroom.Types
  ( PartialConfiguration(..)
  , PartialHeaderConfig(..)
  , PartialHeadersConfig(..)
  , Configuration(..)
  , HeaderConfig(..)
  , HeadersConfig(..)
  , ApplicationError(..)
  , CommandGenError(..)
  , CommandInitError(..)
  , CommandInitOptions(..)
  , CommandRunOptions(..)
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

import           Data.Aeson                     ( FromJSON(..)
                                                , Value(String)
                                                , withObject
                                                , (.!=)
                                                , (.:?)
                                                )
import           Data.Monoid                    ( Last(..) )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
import           RIO
import qualified RIO.Text                      as T


data RunMode
  = Add
  | Drop
  | Replace
  deriving (Eq, Show)

instance FromJSON RunMode where
  parseJSON = \case
    String s -> case T.toLower s of
      "add"     -> pure Add
      "drop"    -> pure Drop
      "replace" -> pure Replace
      _         -> error $ "Unknown run mode: " <> T.unpack s
    other -> error $ "Invalid value for run mode: " <> show other

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
  = InvalidVariable Text
  | NoEndsWith FileType
  | NoFileExtensions FileType
  | NoPutAfter FileType
  | NoRunMode
  | NoSourcePaths
  | NoStartsWith FileType
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
  | Gen Bool (Maybe (LicenseType, FileType))      -- ^ /Generator/ command
  | Init LicenseType [FilePath]                   -- ^ /Init/ command
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

-- | Options for the /Run/ command.
data CommandRunOptions = CommandRunOptions
  { croRunMode       :: RunMode    -- ^ used /Run/ command mode
  , croSourcePaths   :: [FilePath] -- ^ source code file paths
  , croTemplatePaths :: [FilePath] -- ^ template file paths
  , croVariables     :: [Text]     -- ^ raw variables
  , croDebug         :: Bool       -- ^ whether to run in debug mode
  }
  deriving (Eq, Show)

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
  { cRunMode        :: RunMode
  , cSourcePaths    :: [FilePath]
  , cTemplatePaths  :: [FilePath]
  , cVariables      :: HashMap Text Text
  , cLicenseHeaders :: HeadersConfig
  }
  deriving (Eq, Generic, Show)

data HeaderConfig = HeaderConfig
  { hcFileExtensions :: ![Text]
  , hcPutAfter       :: ![Text]
  , hcStartsWith     :: !Text
  , hcEndsWith       :: !Text
  }
  deriving (Eq, Generic, Show)

data HeadersConfig = HeadersConfig
  { hscHaskell :: !HeaderConfig
  , hscHtml    :: !HeaderConfig
  }
  deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------

data PartialConfiguration = PartialConfiguration
  { pcRunMode        :: Last RunMode
  , pcSourcePaths    :: Last [FilePath]
  , pcTemplatePaths  :: Last [FilePath]
  , pcVariables      :: Last (HashMap Text Text)
  , pcLicenseHeaders :: PartialHeadersConfig
  }
  deriving (Eq, Generic, Show)

data PartialHeaderConfig = PartialHeaderConfig
  { phcFileExtensions :: Last [Text]
  , phcPutAfter       :: Last [Text]
  , phcStartsWith     :: Last Text
  , phcEndsWith       :: Last Text
  }
  deriving (Eq, Generic, Show)

data PartialHeadersConfig = PartialHeadersConfig
  { phscHaskell :: PartialHeaderConfig
  , phscHTML    :: PartialHeaderConfig
  }
  deriving (Eq, Generic, Show)

instance FromJSON PartialConfiguration where
  parseJSON = withObject "PartialConfiguration" $ \obj -> do
    pcRunMode        <- Last <$> obj .:? "run-mode"
    pcSourcePaths    <- Last <$> obj .:? "source-paths"
    pcTemplatePaths  <- Last <$> obj .:? "template-paths"
    pcVariables      <- Last <$> obj .:? "variables"
    pcLicenseHeaders <- obj .:? "license-headers" .!= mempty
    pure PartialConfiguration { .. }

instance FromJSON PartialHeaderConfig where
  parseJSON = withObject "PartialHeaderConfig" $ \obj -> do
    phcFileExtensions <- Last <$> obj .:? "file-extensions"
    phcPutAfter       <- Last <$> obj .:? "put-after"
    phcStartsWith     <- Last <$> obj .:? "starts-with"
    phcEndsWith       <- Last <$> obj .:? "ends-with"
    pure PartialHeaderConfig { .. }

instance FromJSON PartialHeadersConfig where
  parseJSON = withObject "PartialHeadersConfig" $ \obj -> do
    phscHaskell <- obj .:? "haskell" .!= mempty
    phscHTML    <- obj .:? "html" .!= mempty
    pure PartialHeadersConfig { .. }

instance Semigroup PartialConfiguration where
  x <> y = PartialConfiguration
    { pcRunMode        = pcRunMode x <> pcRunMode y
    , pcSourcePaths    = pcSourcePaths x <> pcSourcePaths y
    , pcTemplatePaths  = pcTemplatePaths x <> pcTemplatePaths y
    , pcVariables      = pcVariables x <> pcVariables y
    , pcLicenseHeaders = pcLicenseHeaders x <> pcLicenseHeaders y
    }

instance Semigroup PartialHeaderConfig where
  x <> y = PartialHeaderConfig
    { phcFileExtensions = phcFileExtensions x <> phcFileExtensions y
    , phcPutAfter       = phcPutAfter x <> phcPutAfter y
    , phcStartsWith     = phcStartsWith x <> phcStartsWith y
    , phcEndsWith       = phcEndsWith x <> phcEndsWith y
    }

instance Semigroup PartialHeadersConfig where
  x <> y = PartialHeadersConfig { phscHaskell = phscHaskell x <> phscHaskell y
                                , phscHTML    = phscHTML x <> phscHTML y
                                }

instance Monoid PartialConfiguration where
  mempty = PartialConfiguration mempty mempty mempty mempty mempty

instance Monoid PartialHeaderConfig where
  mempty = PartialHeaderConfig mempty mempty mempty mempty

instance Monoid PartialHeadersConfig where
  mempty = PartialHeadersConfig mempty mempty

--------------------------------------------------------------------------------
