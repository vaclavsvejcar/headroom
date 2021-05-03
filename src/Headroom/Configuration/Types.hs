{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Headroom.Configuration.Types
Description : Data types for /Headroom/ configuration
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types representing /Headroom/ configuration options.
Related logic is available in "Headroom.Configuration" module.

Data types related to /Headroom/ configuration uses the
<https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67 partial options monoid>
pattern, but instead of defining separate data type for each /phase/
(/partial/ or /complete/ configuration), the /phase/ is expressed by the 'Phase'
data type and related /closed type family/.
-}

module Headroom.Configuration.Types
  ( -- * Error Types
    ConfigurationError(..)
  , ConfigurationKey(..)
    -- * Type Families
  , Phase(..)
  , (:::)
    -- * Data Types
    -- ** Top Level Configuration
  , Configuration(..)
  , CtConfiguration
  , PtConfiguration
  , HeadersConfig(..)
  , CtHeadersConfig
  , PtHeadersConfig
  , HeaderConfig(..)
  , CtHeaderConfig
  , PtHeaderConfig
    -- ** Header Functions
  , CtUpdateCopyrightConfig
  , PtUpdateCopyrightConfig
  , UpdateCopyrightConfig(..)
  , CtPostProcessConfig
  , PtPostProcessConfig
  , PostProcessConfig(..)
  , CtPostProcessConfigs
  , PtPostProcessConfigs
  , PostProcessConfigs(..)
    -- ** Additional Data Types
  , HeaderSyntax(..)
  , GenMode(..)
  , LicenseType(..)
  , RunMode(..)
  )
where

import           Control.Exception                   ( throw )
import           Data.Aeson                          ( FromJSON(..)
                                                     , Value(String)
                                                     , genericParseJSON
                                                     , withObject
                                                     , (.!=)
                                                     , (.:?)
                                                     )
import           Data.Monoid                         ( Last(..) )
import           Data.String.Interpolate             ( i
                                                     , iii
                                                     )
import           Generic.Data                        ( Generically(..) )
import           Headroom.Data.Regex                 ( Regex(..) )
import           Headroom.Data.Serialization         ( aesonOptions )
import           Headroom.FileType.Types             ( FileType )
import           Headroom.Meta                       ( webDocConfigCurr )
import           Headroom.Template.TemplateRef       ( TemplateRef )
import           Headroom.Types                      ( LicenseType(..)
                                                     , fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import qualified RIO.Text                           as T


------------------------------------  Phase  -----------------------------------

-- | Data type representing state of given configuration data type.
data Phase
  = Partial
  -- ^ partial configuration, could be combined with another or validated to
  -- produce the complete configuration
  | Complete
  -- ^ complete configuration, result of combining and validation of partial
  -- configuration


-- | /Closed type family/ used to express the phase of given data type.
type family (p :: Phase) ::: a where
  'Partial  ::: a = Last a
  'Complete ::: a = a


--------------------------------  HeaderSyntax  --------------------------------

-- | Syntax of the license header comment.
data HeaderSyntax
  = BlockComment Regex Regex (Maybe Text)
  -- ^ block (multi-line) comment syntax (e.g. @/* */@)
  | LineComment Regex (Maybe Text)
  -- ^ single line comment syntax (e.g. @//@)
  deriving (Eq, Show)

-- | Internal representation of the block style of 'HeaderSyntax'.
data BlockComment' = BlockComment'
  { bcStartsWith :: Regex
  -- ^ starting pattern (e.g. @/*@)
  , bcEndsWith   :: Regex
  -- ^ ending pattern (e.g. @*/@)
  }
  deriving (Eq, Generic, Show)

instance FromJSON BlockComment' where
  parseJSON = genericParseJSON aesonOptions

-- | Internal representation of the line style of 'HeaderSyntax'.
newtype LineComment' = LineComment'
  { lcPrefixedBy :: Regex
  -- ^ prefix of the comment line (e.g. @//@)
  }
  deriving (Eq, Generic, Show)

instance FromJSON LineComment' where
  parseJSON = genericParseJSON aesonOptions


-----------------------------------  RunMode  ----------------------------------

-- | Represents what action should the @run@ command perform.
data RunMode
  = Add
  -- ^ /add mode/ for @run@ command
  | Check
  -- ^ /check mode/ for @run@ command
  | Drop
  -- ^ /drop mode/ for @run@ command
  | Replace
  -- ^ /replace mode/ for @run@ command
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


-----------------------------------  GenMode  ----------------------------------

-- | Represents what action should the @gen@ command perform.
data GenMode
  = GenConfigFile
  -- ^ generate /YAML/ config file stub
  | GenLicense (LicenseType, FileType)
  -- ^ generate license header template
  deriving (Eq, Show)


----------------------------  UpdateCopyrightConfig  ---------------------------

-- | Main configuration for the "Headroom.PostProcess.UpdateCopyright"
-- /post-processor/.
data UpdateCopyrightConfig (p :: Phase) = UpdateCopyrightConfig
  { uccSelectedAuthors :: p ::: Maybe (NonEmpty Text)
  -- ^ if specified, years will be updated only in copyright statements of
  -- given authors
  }

-- | Alias for complete variant of 'UpdateCopyrightConfig'.
type CtUpdateCopyrightConfig = UpdateCopyrightConfig 'Complete

-- | Alias for partial variant of 'UpdateCopyrightConfig'.
type PtUpdateCopyrightConfig = UpdateCopyrightConfig 'Partial

deriving instance Eq CtUpdateCopyrightConfig
deriving instance Eq PtUpdateCopyrightConfig
deriving instance Show CtUpdateCopyrightConfig
deriving instance Show PtUpdateCopyrightConfig
deriving instance Generic PtUpdateCopyrightConfig

deriving via (Generically PtUpdateCopyrightConfig)
         instance Semigroup PtUpdateCopyrightConfig
deriving via (Generically PtUpdateCopyrightConfig)
         instance Monoid PtUpdateCopyrightConfig

instance FromJSON PtUpdateCopyrightConfig where
  parseJSON = withObject "PtUpdateCopyrightConfig" $ \obj -> do
    uccSelectedAuthors <- Last <$> obj .:? "selected-authors-only"
    pure UpdateCopyrightConfig { .. }

-------------------------------  PostProcessConfig  -------------------------------

-- | Configuration for selected /post-processor/.
data PostProcessConfig (p :: Phase) c = PostProcessConfig
  { ppcEnabled :: p ::: Bool -- ^ whether this function is enabled or not
  , ppcConfig  :: c p        -- ^ custom configuration of the /post-processor/
  }

-- | Alias for complete variant of 'PostProcessConfig'.
type CtPostProcessConfig c = PostProcessConfig 'Complete c

-- | Alias for partial variant of 'PostProcessConfig'.
type PtPostProcessConfig c = PostProcessConfig 'Partial c


deriving instance (Eq (c 'Complete)) => Eq (CtPostProcessConfig c)
deriving instance (Eq (c 'Partial)) => Eq (PtPostProcessConfig c)
deriving instance (Show (c 'Complete)) => Show (CtPostProcessConfig c)
deriving instance (Show (c 'Partial)) => Show (PtPostProcessConfig c)
deriving instance Generic (PtPostProcessConfig c)

deriving via (Generically (PtPostProcessConfig c))
         instance Semigroup (c 'Partial) => Semigroup (PtPostProcessConfig c)
deriving via (Generically (PtPostProcessConfig c))
         instance Monoid (c 'Partial) => Monoid (PtPostProcessConfig c)

instance (FromJSON (c 'Partial), Monoid (c 'Partial)) => FromJSON (PtPostProcessConfig c) where
  parseJSON = withObject "PtPostProcessConfig" $ \obj -> do
    ppcEnabled <- Last <$> obj .:? "enabled"
    ppcConfig  <- obj .:? "config" .!= mempty
    pure PostProcessConfig { .. }


-------------------------------  PostProcessConfigs  ------------------------------

-- | Configuration of all known /post-processors/.
data PostProcessConfigs (p :: Phase) = PostProcessConfigs
  { ppcsUpdateCopyright :: PostProcessConfig p UpdateCopyrightConfig
  -- ^ configuration for the "Headroom.PostProcess.UpdateCopyright"
  }

-- | Alias for complete variant of 'PostProcessConfigs'.
type CtPostProcessConfigs = PostProcessConfigs 'Complete

-- | Alias for partial variant of 'PostProcessConfigs'.
type PtPostProcessConfigs = PostProcessConfigs 'Partial

deriving instance Eq CtPostProcessConfigs
deriving instance Eq PtPostProcessConfigs
deriving instance Show CtPostProcessConfigs
deriving instance Show PtPostProcessConfigs
deriving instance Generic PtPostProcessConfigs

deriving via (Generically PtPostProcessConfigs)
         instance Semigroup PtPostProcessConfigs
deriving via (Generically PtPostProcessConfigs)
         instance Monoid PtPostProcessConfigs

instance FromJSON PtPostProcessConfigs where
  parseJSON = withObject "PtPostProcessConfigs" $ \obj -> do
    ppcsUpdateCopyright <- obj .:? "update-copyright" .!= mempty
    pure PostProcessConfigs { .. }


--------------------------------  Configuration  -------------------------------

-- | Application configuration.
data Configuration (p :: Phase) = Configuration
  { cRunMode             :: p ::: RunMode
  -- ^ mode of the @run@ command
  , cSourcePaths         :: p ::: [FilePath]
  -- ^ paths to source code files
  , cExcludedPaths       :: p ::: [Regex]
  -- ^ excluded source paths
  , cExcludeIgnoredPaths :: p ::: Bool
  -- ^ whether to exclude paths ignored by VCS
  , cBuiltInTemplates    :: p ::: Maybe LicenseType
  -- ^ used built-in templates
  , cTemplateRefs        :: [TemplateRef]
  -- ^ template references
  , cVariables           :: Variables
  -- ^ variable values for templates
  , cLicenseHeaders      :: HeadersConfig p
  -- ^ configuration of license headers
  , cPostProcessConfigs  :: PostProcessConfigs p
  -- ^ configuration of post-processors
  }

-- | Alias for complete variant of 'Configuration'.
type CtConfiguration = Configuration 'Complete

-- | Alias for partial variant of 'Configuration'.
type PtConfiguration = Configuration 'Partial

deriving instance Eq CtConfiguration
deriving instance Eq PtConfiguration
deriving instance Show CtConfiguration
deriving instance Show PtConfiguration
deriving instance Generic PtConfiguration

deriving via (Generically PtConfiguration)
         instance Semigroup PtConfiguration
deriving via (Generically PtConfiguration)
         instance Monoid PtConfiguration


instance FromJSON PtConfiguration where
  parseJSON = withObject "PtConfiguration" $ \obj -> do
    cRunMode             <- Last <$> obj .:? "run-mode"
    cSourcePaths         <- Last <$> obj .:? "source-paths"
    cExcludedPaths       <- Last <$> obj .:? "excluded-paths"
    cExcludeIgnoredPaths <- Last <$> obj .:? "exclude-ignored-paths"
    cBuiltInTemplates    <- Last <$> obj .:? "builtin-templates"
    cTemplateRefs        <- obj .:? "template-paths" .!= mempty
    cVariables           <- Variables <$> obj .:? "variables" .!= mempty
    cLicenseHeaders      <- obj .:? "license-headers" .!= mempty
    cPostProcessConfigs  <- obj .:? "post-process" .!= mempty
    pure Configuration { .. }


--------------------------------  HeaderConfig  --------------------------------

-- | Configuration for specific license header.
data HeaderConfig (p :: Phase) = HeaderConfig
  { hcFileExtensions   :: p ::: [Text]
  -- ^ list of file extensions (without dot)
  , hcMarginTopCode    :: p ::: Int
  -- ^ margin between header top and preceding code (if present)
  , hcMarginTopFile    :: p ::: Int
  -- ^ margin between header top and start of file (if no code is between)
  , hcMarginBottomCode :: p ::: Int
  -- ^ margin between header bottom and following code (if present)
  , hcMarginBottomFile :: p ::: Int
  -- ^ margin between header bottom and end of file (if no code is between)
  , hcPutAfter         :: p ::: [Regex]
  -- ^ /regexp/ patterns after which to put the header
  , hcPutBefore        :: p ::: [Regex]
  -- ^ /regexp/ patterns before which to put the header
  , hcHeaderSyntax     :: p ::: HeaderSyntax
  -- ^ syntax of the license header comment
  }

-- | Alias for complete variant of 'HeaderConfig'.
type CtHeaderConfig = HeaderConfig 'Complete

-- | Alias for partial variant of 'HeaderConfig'.
type PtHeaderConfig = HeaderConfig 'Partial

deriving instance Eq CtHeaderConfig
deriving instance Eq PtHeaderConfig
deriving instance Show CtHeaderConfig
deriving instance Show PtHeaderConfig
deriving instance Generic PtHeaderConfig

deriving via (Generically PtHeaderConfig)
         instance Semigroup PtHeaderConfig
deriving via (Generically PtHeaderConfig)
         instance Monoid PtHeaderConfig

instance FromJSON PtHeaderConfig where
  parseJSON = withObject "PartialHeaderConfig" $ \obj -> do
    hcFileExtensions   <- Last <$> obj .:? "file-extensions"
    hcMarginTopCode    <- Last <$> obj .:? "margin-top-code"
    hcMarginTopFile    <- Last <$> obj .:? "margin-top-file"
    hcMarginBottomCode <- Last <$> obj .:? "margin-bottom-code"
    hcMarginBottomFile <- Last <$> obj .:? "margin-bottom-file"
    hcPutAfter         <- Last <$> obj .:? "put-after"
    hcPutBefore        <- Last <$> obj .:? "put-before"
    blockComment       <- obj .:? "block-comment"
    lineComment        <- obj .:? "line-comment"
    hcHeaderSyntax     <- pure . Last $ syntax blockComment lineComment
    pure HeaderConfig { .. }
   where
    syntax (Just (BlockComment' s e)) Nothing = Just $ BlockComment s e Nothing
    syntax Nothing (Just (LineComment' p)) = Just $ LineComment p Nothing
    syntax Nothing Nothing = Nothing
    syntax _ _ = throw MixedHeaderSyntax

--------------------------------  HeadersConfig  -------------------------------

-- | Group of 'HeaderConfig' configurations for supported file types.
data HeadersConfig (p :: Phase) = HeadersConfig
  { hscC          :: HeaderConfig p
  -- ^ configuration for /C/ programming language
  , hscCpp        :: HeaderConfig p
  -- ^ configuration for /C++/ programming language
  , hscCss        :: HeaderConfig p
  -- ^ configuration for /CSS/
  , hscGo         :: HeaderConfig p
  -- ^ configuration for /Go/
  , hscHaskell    :: HeaderConfig p
  -- ^ configuration for /Haskell/ programming language
  , hscHtml       :: HeaderConfig p
  -- ^ configuration for /HTML/
  , hscJava       :: HeaderConfig p
  -- ^ configuration for /Java/ programming language
  , hscJs         :: HeaderConfig p
  -- ^ configuration for /JavaScript/ programming language
  , hscPureScript :: HeaderConfig p
  -- ^ configuration for /PureScript/ programming language
  , hscRust       :: HeaderConfig p
  -- ^ configuration for /Rust/ programming language
  , hscScala      :: HeaderConfig p
  -- ^ configuration for /Scala/ programming language
  , hscShell      :: HeaderConfig p
  -- ^ configuration for /Shell/
  }

-- | Alias for complete variant of 'HeadersConfig'.
type CtHeadersConfig = HeadersConfig 'Complete

-- | Alias for partial variant of 'HeadersConfig'.
type PtHeadersConfig = HeadersConfig 'Partial

deriving instance Eq CtHeadersConfig
deriving instance Eq PtHeadersConfig
deriving instance Show CtHeadersConfig
deriving instance Show PtHeadersConfig
deriving instance Generic PtHeadersConfig

deriving via (Generically PtHeadersConfig)
         instance Semigroup PtHeadersConfig
deriving via (Generically PtHeadersConfig)
         instance Monoid PtHeadersConfig

instance FromJSON PtHeadersConfig where
  parseJSON = withObject "PartialHeadersConfig" $ \obj -> do
    hscC          <- obj .:? "c" .!= mempty
    hscCpp        <- obj .:? "cpp" .!= mempty
    hscCss        <- obj .:? "css" .!= mempty
    hscGo         <- obj .:? "go" .!= mempty
    hscHaskell    <- obj .:? "haskell" .!= mempty
    hscHtml       <- obj .:? "html" .!= mempty
    hscJava       <- obj .:? "java" .!= mempty
    hscJs         <- obj .:? "js" .!= mempty
    hscPureScript <- obj .:? "purescript" .!= mempty
    hscRust       <- obj .:? "rust" .!= mempty
    hscScala      <- obj .:? "scala" .!= mempty
    hscShell      <- obj .:? "shell" .!= mempty
    pure HeadersConfig { .. }


---------------------------------  Error Types  --------------------------------

-- | Represents single key in the configuration.
data ConfigurationKey
  = CkFileExtensions FileType
  -- ^ no configuration for @file-extensions@
  | CkHeaderSyntax FileType
  -- ^ no configuration for header syntax
  | CkMarginTopCode FileType
  -- ^ no configuration for margin between header top and preceding code
  | CkMarginTopFile FileType
  -- ^ no configuration for margin between header top and start of file
  | CkMarginBottomCode FileType
  -- ^ no configuration for margin between header bottom and following code
  | CkMarginBottomFile FileType
  -- ^ no configuration for margin between header bottom and end of file
  | CkPutAfter FileType
  -- ^ no configuration for @put-after@
  | CkPutBefore FileType
  -- ^ no configuration for @put-before@
  | CkRunMode
  -- ^ no configuration for @run-mode@
  | CkSourcePaths
  -- ^ no configuration for @source-paths@
  | CkExcludedPaths
  -- ^ no configuration for @excluded-paths@
  | CkExcludeIgnoredPaths
  -- ^ no configuration for @exclude-ignored-paths@
  | CkBuiltInTemplates
  -- ^ no configuration for built in templates
  | CkVariables
  -- ^ no configuration for @variables@
  | CkEnabled
  -- ^ no configuration for @enabled@
  deriving (Eq, Show)


-- | Exception specific to the "Headroom.Configuration" module.
data ConfigurationError
  = MissingConfiguration ConfigurationKey
  -- ^ some of the required configuration keys has not been specified
  | MixedHeaderSyntax
  -- ^ illegal configuration for 'HeaderSyntax'
  deriving (Eq, Show, Typeable)

instance Exception ConfigurationError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: ConfigurationError -> String
displayException' = T.unpack . \case
  MissingConfiguration key -> case key of
    CkFileExtensions fileType -> missingConfig
      (withFT "file-extensions" fileType)
      (Just "file-extensions")
      Nothing
    CkHeaderSyntax fileType -> missingConfig
      (withFT "comment-syntax" fileType)
      (Just "block-comment|line-comment")
      Nothing
    CkMarginTopCode fileType -> missingConfig
      (withFT "margin-top-code" fileType)
      (Just "margin-top-code")
      Nothing
    CkMarginTopFile fileType -> missingConfig
      (withFT "margin-top-file" fileType)
      (Just "margin-top-file")
      Nothing
    CkMarginBottomCode fileType -> missingConfig
      (withFT "margin-bottom-code" fileType)
      (Just "margin-bottom-code")
      Nothing
    CkMarginBottomFile fileType -> missingConfig
      (withFT "margin-bottom-file" fileType)
      (Just "margin-bottom-file")
      Nothing
    CkPutAfter fileType ->
      missingConfig (withFT "put-after" fileType) (Just "put-after") Nothing
    CkPutBefore fileType ->
      missingConfig (withFT "put-before" fileType) (Just "put-before") Nothing
    CkRunMode -> missingConfig
      "mode of the run command"
      (Just "run-mode")
      (Just
        "(-a|--add-headers)|(-c|--check-header)|(-d|--drop-header)|(-r|--replace-headers)"
      )
    CkSourcePaths -> missingConfig "paths to source code files"
                                   (Just "source-paths")
                                   (Just "-s|--source-path")
    CkExcludedPaths -> missingConfig "excluded paths"
                                     (Just "excluded-paths")
                                     (Just "-e|--excluded-path")
    CkExcludeIgnoredPaths -> missingConfig "whether to exclude ignored paths"
                                           (Just "exclude-ignored-paths")
                                           (Just "--exclude-ignored-paths")
    CkBuiltInTemplates -> missingConfig
      "use of built-in templates"
      (Just "builtin-templates")
      (Just "(-t|--template-path)|--builtin-templates")
    CkVariables -> missingConfig "template variables"
                                 (Just "variables")
                                 (Just "-v|--variable")
    CkEnabled -> missingConfig "enabled" (Just "enabled") Nothing
  MixedHeaderSyntax -> mixedHeaderSyntax
 where
  withFT msg fileType = [i|#{msg :: Text} (#{fileType})|]
  mixedHeaderSyntax = [iii|
      Invalid configuration, combining 'block-comment' with 'line-comment'
      is not allowed. Either use 'block-comment' to define multi-line
      comment header, or 'line-comment' to define header composed of
      multiple single-line comments.
    |]


missingConfig :: Text -> Maybe Text -> Maybe Text -> Text
missingConfig desc yaml cli = [iii|
    Missing configuration for '#{desc}' (#{options}). See following page for
    more details: #{webDocConfigCurr}
  |]
 where
  cliText  = fmap (\c -> [i|command line option '#{c}'|]) cli
  yamlText = fmap (\c -> [i|YAML option '#{c}'|]) yaml
  options  = T.intercalate " or " . catMaybes $ [cliText, yamlText]
