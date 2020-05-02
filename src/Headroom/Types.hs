{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Headroom.Types
Description : Application data types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module containing most of the data types used by the application.
-}

module Headroom.Types
  (
    -- * Configuration Data Types
    -- ** Total Configuration
    Configuration(..)
  , HeaderConfig(..)
  , HeadersConfig(..)
    -- ** Partial Configuration
  , PartialConfiguration(..)
  , PartialHeaderConfig(..)
  , PartialHeadersConfig(..)
    -- ** Other Configuration Data Types
  , HeaderSyntax(..)
    -- * Command Data Types
  , Command(..)
  , CommandGenOptions(..)
  , CommandInitOptions(..)
  , CommandRunOptions(..)
  , ConfigurationError(..)
  , RunAction(..)
  , RunMode(..)
  , GenMode(..)
    -- * Error Data Types
  , ApplicationError(..)
  , CommandGenError(..)
  , CommandInitError(..)
  , TemplateError(..)
    -- * Other Data Types
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
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Serialization         ( aesonOptions )
import           RIO
import qualified RIO.Text                      as T


-- | Action to be performed based on the selected 'RunMode'.
data RunAction = RunAction
  { raProcessed    :: !Bool           -- ^ whether the given file was processed
  , raFunc         :: !(Text -> Text) -- ^ function to process the file
  , raProcessedMsg :: !Text           -- ^ message to show when file was processed
  , raSkippedMsg   :: !Text           -- ^ message to show when file was skipped
  }

-- | Represents what action should the @run@ command perform.
data RunMode
  = Add     -- ^ /add mode/ for @run@ command
  | Check   -- ^ /check mode/ for @run@ command
  | Drop    -- ^ /drop mode/ for @run@ command
  | Replace -- ^ /replace mode/ for @run@ command
  deriving (Eq, Show)

instance FromJSON RunMode where
  parseJSON = \case
    String s -> case T.toLower s of
      "add"     -> pure Add
      "check"   -> pure Check
      "drop"    -> pure Drop
      "replace" -> pure Replace
      _         -> error $ "Unknown run mode: " <> T.unpack s
    other -> error $ "Invalid value for run mode: " <> show other

-- | Represents what action should the @gen@ command perform.
data GenMode
  = GenConfigFile                       -- ^ generate /YAML/ config file stub
  | GenLicense !(LicenseType, FileType) -- ^ generate license header template
  deriving (Eq, Show)

-- | Represents error that can occur during the application execution.
data ApplicationError
  = CommandGenError !CommandGenError       -- ^ error specific for the @gen@ command
  | CommandInitError !CommandInitError     -- ^ error specific for the @init@ command
  | ConfigurationError !ConfigurationError -- ^ error processing configuration
  | TemplateError !TemplateError           -- ^ error processing template
  deriving (Eq, Show)

instance Exception ApplicationError where
  displayException = T.unpack . \case
    CommandGenError    error' -> commandGenError error'
    CommandInitError   error' -> commandInitError error'
    ConfigurationError error' -> configurationError error'
    TemplateError      error' -> templateError error'

-- | Error specific for the @gen@ command.
data CommandGenError = NoGenModeSelected -- ^ no mode of /Gen/ command selected
  deriving (Eq, Show)

-- | Error specific for the @init@ command.
data CommandInitError
  = AppConfigAlreadyExists !FilePath -- ^ application configuration file already exists
  | NoProvidedSourcePaths            -- ^ no paths to source code files provided
  | NoSupportedFileType              -- ^ no supported file types found on source paths
  deriving (Eq, Show)

-- | Error during processing configuration.
data ConfigurationError
  = InvalidVariable !Text      -- ^ invalid variable input (as @key=value@)
  | MixedHeaderSyntax          -- ^ illegal configuration for 'HeaderSyntax'
  | NoFileExtensions !FileType -- ^ no configuration for @file-extensions@
  | NoHeaderSyntax !FileType   -- ^ no configuration for header syntax
  | NoMarginAfter !FileType    -- ^ no configuration for @margin-after@
  | NoMarginBefore !FileType   -- ^ no configuration for @margin-before@
  | NoPutAfter !FileType       -- ^ no configuration for @put-after@
  | NoPutBefore !FileType      -- ^ no configuration for @put-before@
  | NoRunMode                  -- ^ no configuration for @run-mode@
  | NoSourcePaths              -- ^ no configuration for @source-paths@
  | NoExcludedPaths            -- ^ no configuration for @excluded-paths@
  | NoTemplatePaths            -- ^ no configuration for @template-paths@
  | NoVariables                -- ^ no configuration for @variables@
  deriving (Eq, Show)

-- | Error during processing template.
data TemplateError
  = MissingVariables !Text ![Text] -- ^ missing variable values
  | ParseError !Text               -- ^ error parsing raw template text
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Application command.
data Command
  = Run [FilePath] [Text] [FilePath] [Text] (Maybe RunMode) Bool Bool -- ^ @run@ command
  | Gen Bool (Maybe (LicenseType, FileType))                          -- ^ @gen@ command
  | Init LicenseType [FilePath]                                       -- ^ @init@ command
  deriving (Show)

--------------------------------------------------------------------------------

-- | Options for the @gen@ command.
newtype CommandGenOptions = CommandGenOptions
  { cgoGenMode :: GenMode -- ^ selected mode
  }
  deriving (Show)

-- | Options for the @init@ command.
data CommandInitOptions = CommandInitOptions
  { cioSourcePaths :: ![FilePath]  -- ^ paths to source code files
  , cioLicenseType :: !LicenseType -- ^ license type
  }
  deriving Show

-- | Options for the @run@ command.
data CommandRunOptions = CommandRunOptions
  { croRunMode       :: !(Maybe RunMode) -- ^ used /Run/ command mode
  , croSourcePaths   :: ![FilePath]      -- ^ source code file paths
  , croExcludedPaths :: ![Text]          -- ^ source paths to exclude
  , croTemplatePaths :: ![FilePath]      -- ^ template file paths
  , croVariables     :: ![Text]          -- ^ raw variables
  , croDebug         :: !Bool            -- ^ whether to run in debug mode
  , croDryRun        :: !Bool            -- ^ whether to perform dry run
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Supported type of source code file.
data FileType
  = C       -- ^ support for /C/ programming language
  | CPP     -- ^ support for /C++/ programming language
  | CSS     -- ^ support for /CSS/
  | Haskell -- ^ support for /Haskell/ programming language
  | HTML    -- ^ support for /HTML/
  | Java    -- ^ support for /Java/ programming language
  | JS      -- ^ support for /JavaScript/ programming language
  | Rust    -- ^ support for /Rust/ programming language
  | Scala   -- ^ support for /Scala/ programming language
  | Shell   -- ^ support for /Shell/
  deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Supported type of open source license.
data LicenseType
  = Apache2 -- ^ support for /Apache-2.0/ license
  | BSD3    -- ^ support for /BSD-3-Clause/ license
  | GPL2    -- ^ support for /GNU GPL2/ license
  | GPL3    -- ^ support for /GNU GPL3/ license
  | MIT     -- ^ support for /MIT/ license
  | MPL2    -- ^ support for /MPL2/ license
  deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Info extracted about the concrete source code file.
data FileInfo = FileInfo
  { fiFileType     :: !FileType            -- ^ type of the file
  , fiHeaderConfig :: !HeaderConfig        -- ^ configuration for license header
  , fiHeaderPos    :: !(Maybe (Int, Int))  -- ^ position of existing license header
  , fiVariables    :: !(HashMap Text Text) -- ^ additional extracted variables
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Syntax of the license header comment.
data HeaderSyntax
  = BlockComment !Text !Text -- ^ block (multi-line) comment syntax (e.g. @/* */@)
  | LineComment !Text        -- ^ single line comment syntax (e.g. @//@)
  deriving (Eq, Show)

-- | Application configuration.
data Configuration = Configuration
  { cRunMode        :: !RunMode             -- ^ mode of the @run@ command
  , cSourcePaths    :: ![FilePath]          -- ^ paths to source code files
  , cExcludedPaths  :: ![Text]              -- ^ excluded source paths
  , cTemplatePaths  :: ![FilePath]          -- ^ paths to template files
  , cVariables      :: !(HashMap Text Text) -- ^ variable values for templates
  , cLicenseHeaders :: !HeadersConfig       -- ^ configuration of license headers
  }
  deriving (Eq, Show)

-- | Configuration for specific license header.
data HeaderConfig = HeaderConfig
  { hcFileExtensions :: ![Text]       -- ^ list of file extensions (without dot)
  , hcMarginAfter    :: !Int          -- ^ number of empty lines to put after header
  , hcMarginBefore   :: !Int          -- ^ number of empty lines to put before header
  , hcPutAfter       :: ![Text]       -- ^ /regexp/ patterns after which to put the header
  , hcPutBefore      :: ![Text]       -- ^ /regexp/ patterns before which to put the header
  , hcHeaderSyntax   :: !HeaderSyntax -- ^ syntax of the license header comment
  }
  deriving (Eq, Show)

-- | Group of 'HeaderConfig' configurations for supported file types.
data HeadersConfig = HeadersConfig
  { hscC       :: !HeaderConfig -- ^ configuration for /C/ programming language
  , hscCpp     :: !HeaderConfig -- ^ configuration for /C++/ programming language
  , hscCss     :: !HeaderConfig -- ^ configuration for /CSS/
  , hscHaskell :: !HeaderConfig -- ^ configuration for /Haskell/ programming language
  , hscHtml    :: !HeaderConfig -- ^ configuration for /HTML/
  , hscJava    :: !HeaderConfig -- ^ configuration for /Java/ programming language
  , hscJs      :: !HeaderConfig -- ^ configuration for /JavaScript/ programming language
  , hscRust    :: !HeaderConfig -- ^ configuration for /Rust/ programming language
  , hscScala   :: !HeaderConfig -- ^ configuration for /Scala/ programming language
  , hscShell   :: !HeaderConfig -- ^ configuration for /Shell/
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Internal representation of block comment style of 'HeaderSyntax'.
data BlockComment' = BlockComment'
  { bcStartsWith :: !Text -- ^ starting pattern (e.g. @/*@)
  , bcEndsWith   :: !Text -- ^ ending pattern (e.g. @*/@)
  }
  deriving (Eq, Generic, Show)

-- | Internal representation of the line style of 'HeaderSyntax'.
newtype LineComment' = LineComment'
  { lcPrefixedBy :: Text -- ^ prefix of the comment line (e.g. @//@)
  }
  deriving (Eq, Generic, Show)

-- | Partial (possibly incomplete) version of 'Configuration'.
data PartialConfiguration = PartialConfiguration
  { pcRunMode        :: !(Last RunMode)             -- ^ mode of the @run@ command
  , pcSourcePaths    :: !(Last [FilePath])          -- ^ paths to source code files
  , pcExcludedPaths  :: !(Last [Text])              -- ^ excluded source paths
  , pcTemplatePaths  :: !(Last [FilePath])          -- ^ paths to template files
  , pcVariables      :: !(Last (HashMap Text Text)) -- ^ variable values for templates
  , pcLicenseHeaders :: !PartialHeadersConfig       -- ^ configuration of license headers
  }
  deriving (Eq, Generic, Show)

-- | Partial (possibly incomplete) version of 'HeaderConfig'.
data PartialHeaderConfig = PartialHeaderConfig
  { phcFileExtensions :: !(Last [Text])       -- ^ list of file extensions (without dot)
  , phcMarginAfter    :: !(Last Int)          -- ^ number of empty lines to put after header
  , phcMarginBefore   :: !(Last Int)          -- ^ number of empty lines to put before header
  , phcPutAfter       :: !(Last [Text])       -- ^ /regexp/ patterns after which to put the header
  , phcPutBefore      :: !(Last [Text])       -- ^ /regexp/ patterns before which to put the header
  , phcHeaderSyntax   :: !(Last HeaderSyntax) -- ^ syntax of the license header comment
  }
  deriving (Eq, Generic, Show)

-- | Partial (possibly incomplete) version of 'HeadersConfig'.
data PartialHeadersConfig = PartialHeadersConfig
  { phscC       :: !PartialHeaderConfig -- ^ configuration for /C/ programming language
  , phscCpp     :: !PartialHeaderConfig -- ^ configuration for /C++/ programming language
  , phscCss     :: !PartialHeaderConfig -- ^ configuration for /CSS/
  , phscHaskell :: !PartialHeaderConfig -- ^ configuration for /Haskell/ programming language
  , phscHtml    :: !PartialHeaderConfig -- ^ configuration for /HTML/
  , phscJava    :: !PartialHeaderConfig -- ^ configuration for /Java/ programming language
  , phscJs      :: !PartialHeaderConfig -- ^ configuration for /JavaScript/ programming language
  , phscRust    :: !PartialHeaderConfig -- ^ configuration for /Rust/ programming language
  , phscScala   :: !PartialHeaderConfig -- ^ configuration for /Scala/ programming language
  , phscShell   :: !PartialHeaderConfig -- ^ configuration for /Shell/
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
    pcExcludedPaths  <- Last <$> obj .:? "excluded-paths"
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
    phscC       <- obj .:? "c" .!= mempty
    phscCpp     <- obj .:? "cpp" .!= mempty
    phscCss     <- obj .:? "css" .!= mempty
    phscHaskell <- obj .:? "haskell" .!= mempty
    phscHtml    <- obj .:? "html" .!= mempty
    phscJava    <- obj .:? "java" .!= mempty
    phscJs      <- obj .:? "js" .!= mempty
    phscRust    <- obj .:? "rust" .!= mempty
    phscScala   <- obj .:? "scala" .!= mempty
    phscShell   <- obj .:? "shell" .!= mempty
    pure PartialHeadersConfig { .. }

instance Semigroup PartialConfiguration where
  x <> y = PartialConfiguration
    { pcRunMode        = pcRunMode x <> pcRunMode y
    , pcSourcePaths    = pcSourcePaths x <> pcSourcePaths y
    , pcExcludedPaths  = pcExcludedPaths x <> pcExcludedPaths y
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
  x <> y = PartialHeadersConfig { phscC       = phscC x <> phscC y
                                , phscCpp     = phscCpp x <> phscCpp y
                                , phscCss     = phscCss x <> phscCss y
                                , phscHaskell = phscHaskell x <> phscHaskell y
                                , phscHtml    = phscHtml x <> phscHtml y
                                , phscJava    = phscJava x <> phscJava y
                                , phscJs      = phscJs x <> phscJs y
                                , phscRust    = phscRust x <> phscRust y
                                , phscScala   = phscScala x <> phscScala y
                                , phscShell   = phscShell x <> phscShell y
                                }

instance Monoid PartialConfiguration where
  mempty = PartialConfiguration mempty mempty mempty mempty mempty mempty

instance Monoid PartialHeaderConfig where
  mempty = PartialHeaderConfig mempty mempty mempty mempty mempty mempty

instance Monoid PartialHeadersConfig where
  mempty = PartialHeadersConfig mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty

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
  NoExcludedPaths           -> noFlag "excluded-paths"
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
