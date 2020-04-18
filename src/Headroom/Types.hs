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
  , HeaderSyntax(..)
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
  , FileInfo(..)
  )
where

import           Control.Exception              ( throw )
import           Data.Aeson                     ( FromJSON(..)
                                                , Value(String)
                                                , genericParseJSON
                                                , withObject
                                                , (.!=)
                                                , (.:?)
                                                )
import           Data.Monoid                    ( Last(..) )
import           Headroom.Serialization         ( aesonOptions )
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
  = GenConfigFile                      -- ^ generate /YAML/ config file stub
  | GenLicense (LicenseType, FileType) -- ^ generate license header template
  deriving (Eq, Show)

data ApplicationError
  = CommandGenError CommandGenError
  | CommandInitError CommandInitError
  | ConfigurationError ConfigurationError
  | TemplateError TemplateError
  deriving (Eq, Show)

instance Exception ApplicationError where
  displayException = T.unpack . \case
    CommandGenError    error' -> commandGenError error'
    CommandInitError   error' -> commandInitError error'
    ConfigurationError error' -> configurationError error'
    TemplateError      error' -> templateError error'

-- | Errors specific for the /Gen/ command.
data CommandGenError = NoGenModeSelected -- ^ no mode of /Gen/ command selected
  deriving (Eq, Show)

data CommandInitError
  = AppConfigAlreadyExists FilePath -- ^ application configuration file already exists
  | NoProvidedSourcePaths           -- ^ no paths to source code files provided
  | NoSupportedFileType             -- ^ no supported file types found on source paths
  deriving (Eq, Show)

data ConfigurationError
  = InvalidVariable Text
  | MixedHeaderSyntax
  | NoFileExtensions FileType
  | NoHeaderSyntax FileType
  | NoMarginAfter FileType
  | NoMarginBefore FileType
  | NoPutAfter FileType
  | NoPutBefore FileType
  | NoRunMode
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
  { croRunMode       :: !RunMode    -- ^ used /Run/ command mode
  , croSourcePaths   :: ![FilePath] -- ^ source code file paths
  , croTemplatePaths :: ![FilePath] -- ^ template file paths
  , croVariables     :: ![Text]     -- ^ raw variables
  , croDebug         :: !Bool       -- ^ whether to run in debug mode
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data FileType
  = CSS
  | Haskell
  | HTML
  | Java
  | JS
  | Rust
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

data FileInfo = FileInfo
  { fiFileType     :: !FileType
  , fiHeaderConfig :: !HeaderConfig
  , fiHeaderPos    :: !(Maybe (Int, Int))
  , fiVariables    :: !(HashMap Text Text)
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data HeaderSyntax
  = BlockComment !Text !Text
  | LineComment !Text
  deriving (Eq, Show)

data Configuration = Configuration
  { cRunMode        :: !RunMode
  , cSourcePaths    :: ![FilePath]
  , cTemplatePaths  :: ![FilePath]
  , cVariables      :: !(HashMap Text Text)
  , cLicenseHeaders :: !HeadersConfig
  }
  deriving (Eq, Show)

data HeaderConfig = HeaderConfig
  { hcFileExtensions :: ![Text]
  , hcMarginAfter    :: !Int
  , hcMarginBefore   :: !Int
  , hcPutAfter       :: ![Text]
  , hcPutBefore      :: ![Text]
  , hcHeaderSyntax   :: !HeaderSyntax
  }
  deriving (Eq, Show)

data HeadersConfig = HeadersConfig
  { hscCss     :: !HeaderConfig
  , hscHaskell :: !HeaderConfig
  , hscHtml    :: !HeaderConfig
  , hscJava    :: !HeaderConfig
  , hscJs      :: !HeaderConfig
  , hscRust    :: !HeaderConfig
  , hscScala   :: !HeaderConfig
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data BlockComment' = BlockComment'
  { bcStartsWith :: !Text
  , bcEndsWith   :: Text
  }
  deriving (Eq, Generic, Show)

newtype LineComment' = LineComment'
  { lcPrefixedBy :: Text
  }
  deriving (Eq, Generic, Show)

data PartialConfiguration = PartialConfiguration
  { pcRunMode        :: !(Last RunMode)
  , pcSourcePaths    :: !(Last [FilePath])
  , pcTemplatePaths  :: !(Last [FilePath])
  , pcVariables      :: !(Last (HashMap Text Text))
  , pcLicenseHeaders :: !PartialHeadersConfig
  }
  deriving (Eq, Generic, Show)

data PartialHeaderConfig = PartialHeaderConfig
  { phcFileExtensions :: !(Last [Text])
  , phcMarginAfter    :: !(Last Int)
  , phcMarginBefore   :: !(Last Int)
  , phcPutAfter       :: !(Last [Text])
  , phcPutBefore      :: !(Last [Text])
  , phcHeaderSyntax   :: !(Last HeaderSyntax)
  }
  deriving (Eq, Generic, Show)

data PartialHeadersConfig = PartialHeadersConfig
  { phscCss     :: !PartialHeaderConfig
  , phscHaskell :: !PartialHeaderConfig
  , phscHtml    :: !PartialHeaderConfig
  , phscJava    :: !PartialHeaderConfig
  , phscJs      :: !PartialHeaderConfig
  , phscRust    :: !PartialHeaderConfig
  , phscScala   :: !PartialHeaderConfig
  }
  deriving (Eq, Generic, Show)

instance FromJSON BlockComment' where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON LineComment' where
  parseJSON = genericParseJSON aesonOptions

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
    phcMarginAfter    <- Last <$> obj .:? "margin-after"
    phcMarginBefore   <- Last <$> obj .:? "margin-before"
    phcPutAfter       <- Last <$> obj .:? "put-after"
    phcPutBefore      <- Last <$> obj .:? "put-before"
    blockComment      <- obj .:? "block-comment"
    lineComment       <- obj .:? "line-comment"
    let phcHeaderSyntax = Last $ headerSyntax blockComment lineComment
    pure PartialHeaderConfig { .. }
   where
    headerSyntax (Just (BlockComment' s e)) Nothing = Just $ BlockComment s e
    headerSyntax Nothing (Just (LineComment' p)) = Just $ LineComment p
    headerSyntax Nothing Nothing = Nothing
    headerSyntax _ _ = throw error'
    error' = ConfigurationError MixedHeaderSyntax

instance FromJSON PartialHeadersConfig where
  parseJSON = withObject "PartialHeadersConfig" $ \obj -> do
    phscCss     <- obj .:? "css" .!= mempty
    phscHaskell <- obj .:? "haskell" .!= mempty
    phscHtml    <- obj .:? "html" .!= mempty
    phscJava    <- obj .:? "java" .!= mempty
    phscJs      <- obj .:? "js" .!= mempty
    phscRust    <- obj .:? "rust" .!= mempty
    phscScala   <- obj .:? "scala" .!= mempty
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
    , phcMarginAfter    = phcMarginAfter x <> phcMarginAfter y
    , phcMarginBefore   = phcMarginBefore x <> phcMarginBefore y
    , phcPutAfter       = phcPutAfter x <> phcPutAfter y
    , phcPutBefore      = phcPutBefore x <> phcPutBefore y
    , phcHeaderSyntax   = phcHeaderSyntax x <> phcHeaderSyntax y
    }

instance Semigroup PartialHeadersConfig where
  x <> y = PartialHeadersConfig { phscCss     = phscCss x <> phscCss y
                                , phscHaskell = phscHaskell x <> phscHaskell y
                                , phscHtml    = phscHtml x <> phscHtml y
                                , phscJava    = phscJava x <> phscJava y
                                , phscJs      = phscJs x <> phscJs y
                                , phscRust    = phscRust x <> phscRust y
                                , phscScala   = phscScala x <> phscScala y
                                }

instance Monoid PartialConfiguration where
  mempty = PartialConfiguration mempty mempty mempty mempty mempty

instance Monoid PartialHeaderConfig where
  mempty = PartialHeaderConfig mempty mempty mempty mempty mempty mempty

instance Monoid PartialHeadersConfig where
  mempty =
    PartialHeadersConfig mempty mempty mempty mempty mempty mempty mempty

--------------------------------------------------------------------------------

commandGenError :: CommandGenError -> Text
commandGenError = \case
  NoGenModeSelected -> noGenModeSelected
 where
  noGenModeSelected = mconcat
    [ "Please select at least one option what to generate "
    , "(see --help for details)"
    ]

commandInitError :: CommandInitError -> Text
commandInitError = \case
  AppConfigAlreadyExists path -> appConfigAlreadyExists path
  NoProvidedSourcePaths       -> noProvidedSourcePaths
  NoSupportedFileType         -> noSupportedFileType
 where
  appConfigAlreadyExists path =
    mconcat ["Configuration file '", T.pack path, "' already exists"]
  noProvidedSourcePaths = "No source code paths (files or directories) defined"
  noSupportedFileType   = "No supported file type found in scanned source paths"

configurationError :: ConfigurationError -> Text
configurationError = \case
  InvalidVariable input     -> invalidVariable input
  MixedHeaderSyntax         -> mixedHeaderSyntax
  NoFileExtensions fileType -> noProp "file-extensions" fileType
  NoHeaderSyntax   fileType -> noProp "block-comment/line-comment" fileType
  NoMarginAfter    fileType -> noProp "margin-after" fileType
  NoMarginBefore   fileType -> noProp "margin-before" fileType
  NoPutAfter       fileType -> noProp "put-after" fileType
  NoPutBefore      fileType -> noProp "put-before" fileType
  NoRunMode                 -> noFlag "run-mode"
  NoSourcePaths             -> noFlag "source-paths"
  NoTemplatePaths           -> noFlag "template-paths"
  NoVariables               -> noFlag "variables"
 where
  invalidVariable = ("Cannot parse variable key=value from: " <>)
  noProp prop fileType = T.pack $ mconcat
    ["Missing '", prop, "' configuration key for file type", show fileType]
  noFlag flag = mconcat ["Missing configuration key: ", flag]
  mixedHeaderSyntax = mconcat
    [ "Invalid configuration, combining 'block-comment' with 'line-comment' "
    , "is not allowed. Either use 'block-comment' to define multi-line "
    , "comment header, or 'line-comment' to define header composed of "
    , "multiple single-line comments."
    ]

templateError :: TemplateError -> Text
templateError = \case
  MissingVariables name variables -> missingVariables name variables
  ParseError msg                  -> parseError msg
 where
  missingVariables name variables = mconcat
    ["Missing variables for template '", name, "': ", T.pack $ show variables]
  parseError msg = "Error parsing template: " <> msg
