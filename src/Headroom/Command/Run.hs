{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

{-|
Module      : Headroom.Command.Run
Description : Handler for the @run@ command.
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module representing the @run@ command, the core command of /Headroom/, which is
responsible for license header management.
-}

module Headroom.Command.Run
  ( commandRun
  , loadBuiltInTemplates
  , loadTemplateFiles
  , typeOfTemplate
    -- * License Header Post-processing
  , postProcessHeader'
  , sanitizeHeader
  )
where

import           Data.Time.Calendar                  ( toGregorian )
import           Data.Time.Clock                     ( getCurrentTime )
import           Data.Time.Clock.POSIX               ( getPOSIXTime )
import           Data.Time.LocalTime                 ( getCurrentTimeZone
                                                     , localDay
                                                     , utcToLocalTime
                                                     )
import           Headroom.Command.Types              ( CommandRunOptions(..) )
import           Headroom.Command.Utils              ( bootstrap )
import           Headroom.Configuration              ( loadConfiguration
                                                     , makeConfiguration
                                                     , parseConfiguration
                                                     )
import           Headroom.Configuration.Types        ( Configuration(..)
                                                     , CtConfiguration
                                                     , CtHeaderFnConfigs
                                                     , HeaderConfig(..)
                                                     , HeaderSyntax(..)
                                                     , LicenseType(..)
                                                     , PtConfiguration
                                                     , RunMode(..)
                                                     , TemplateSource(..)
                                                     )
import           Headroom.Data.EnumExtra             ( EnumExtra(..) )
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Lens                  ( suffixLenses
                                                     , suffixLensesFor
                                                     )
import           Headroom.Data.TextExtra             ( mapLines )
import           Headroom.Embedded                   ( defaultConfig
                                                     , licenseTemplate
                                                     )
import           Headroom.FileSystem                 ( FileSystem(..)
                                                     , excludePaths
                                                     , fileExtension
                                                     , mkFileSystem
                                                     )
import           Headroom.FileType                   ( fileTypeByExt )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header                     ( addHeader
                                                     , dropHeader
                                                     , extractFileInfo
                                                     , replaceHeader
                                                     )
import           Headroom.Header.TemplateInfo        ( mkTemplateInfo )
import           Headroom.Header.Types               ( FileInfo(..)
                                                     , TemplateInfo(..)
                                                     )
import           Headroom.HeaderFn                   ( mkConfiguredEnv
                                                     , postProcessHeader
                                                     )
import           Headroom.Meta                       ( TemplateType
                                                     , configFileName
                                                     , productInfo
                                                     )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Types                      ( CurrentYear(..) )
import           Headroom.UI                         ( Progress(..)
                                                     , zipWithProgress
                                                     )
import           Headroom.Variables                  ( compileVariables
                                                     , dynamicVariables
                                                     , parseVariables
                                                     )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import           RIO.FilePath                        ( takeBaseName )
import qualified RIO.List                           as L
import qualified RIO.Map                            as M
import qualified RIO.Text                           as T


suffixLensesFor ["cHeaderFnConfigs"] ''Configuration


-- | Action to be performed based on the selected 'RunMode'.
data RunAction = RunAction
  { raProcessed    :: Bool
  -- ^ whether the given file was processed
  , raFunc         :: Text -> Text
  -- ^ function to process the file
  , raProcessedMsg :: Text
  -- ^ message to show when file was processed
  , raSkippedMsg   :: Text
  -- ^ message to show when file was skipped
  }


-- | Initial /RIO/ startup environment for the /Run/ command.
data StartupEnv = StartupEnv
  { envLogFunc    :: LogFunc
  -- ^ logging function
  , envRunOptions :: CommandRunOptions
  -- ^ options
  }

suffixLenses ''StartupEnv

-- | Full /RIO/ environment for the /Run/ command.
data Env = Env
  { envEnv           :: StartupEnv
  -- ^ startup /RIO/ environment
  , envConfiguration :: CtConfiguration
  -- ^ application configuration
  , envCurrentYear   :: CurrentYear
  -- ^ current year
  , envFileSystem    :: FileSystem (RIO Env)
  -- ^ file system operations
  }

suffixLenses ''Env

instance Has CtConfiguration Env where
  hasLens = envConfigurationL

instance Has CtHeaderFnConfigs Env where
  hasLens = envConfigurationL . cHeaderFnConfigsL

instance Has StartupEnv StartupEnv where
  hasLens = id

instance Has StartupEnv Env where
  hasLens = envEnvL

instance HasLogFunc StartupEnv where
  logFuncL = envLogFuncL

instance HasLogFunc Env where
  logFuncL = hasLens @StartupEnv . logFuncL

instance Has CommandRunOptions StartupEnv where
  hasLens = envRunOptionsL

instance Has CommandRunOptions Env where
  hasLens = hasLens @StartupEnv . hasLens

instance Has CurrentYear Env where
  hasLens = envCurrentYearL

instance Has (FileSystem (RIO Env)) Env where
  hasLens = envFileSystemL


env' :: CommandRunOptions -> LogFunc -> IO Env
env' opts logFunc = do
  let envEnv        = StartupEnv { envLogFunc = logFunc, envRunOptions = opts }
      envFileSystem = mkFileSystem
  envConfiguration <- runRIO envEnv finalConfiguration
  envCurrentYear   <- currentYear
  pure Env { .. }


-- | Handler for /Run/ command.
commandRun :: CommandRunOptions
           -- ^ /Run/ command options
           -> IO ()
           -- ^ execution result
commandRun opts = bootstrap (env' opts) (croDebug opts) $ do
  CommandRunOptions {..} <- viewL
  Configuration {..}     <- viewL @CtConfiguration
  let isCheck = cRunMode == Check
  warnOnDryRun
  startTS            <- liftIO getPOSIXTime
  templates          <- loadTemplates
  sourceFiles        <- findSourceFiles (M.keys templates)
  (total, processed) <- processSourceFiles templates sourceFiles
  endTS              <- liftIO getPOSIXTime
  logInfo "-----"
  logInfo $ mconcat
    [ "Done: "
    , if isCheck then "outdated " else "modified "
    , display processed
    , if isCheck then ", up-to-date " else ", skipped "
    , display (total - processed)
    , " file(s) in "
    , displayShow (endTS - startTS)
    , " second(s)."
    ]
  warnOnDryRun
  when (not croDryRun && isCheck && processed > 0) (exitWith $ ExitFailure 1)


warnOnDryRun :: (HasLogFunc env, Has CommandRunOptions env) => RIO env ()
warnOnDryRun = do
  CommandRunOptions {..} <- viewL
  when croDryRun $ logWarn "[!] Running with '--dry-run', no files are changed!"


findSourceFiles :: ( Has CtConfiguration env
                   , Has (FileSystem (RIO env)) env
                   , HasLogFunc env
                   )
                => [FileType]
                -> RIO env [FilePath]
findSourceFiles fileTypes = do
  Configuration {..} <- viewL
  FileSystem {..}    <- viewL
  logDebug $ "Using source paths: " <> displayShow cSourcePaths
  files <-
    mconcat <$> mapM (fsFindFilesByTypes cLicenseHeaders fileTypes) cSourcePaths
  let files' = excludePaths cExcludedPaths files
  logInfo $ mconcat
    [ "Found "
    , display $ L.length files'
    , " source file(s) (excluded "
    , display $ L.length files - L.length files'
    , " file(s))"
    ]
  pure files'


processSourceFiles :: ( Has CtConfiguration env
                      , Has CtHeaderFnConfigs env
                      , Has CommandRunOptions env
                      , Has CurrentYear env
                      , HasLogFunc env
                      )
                   => Map FileType TemplateInfo
                   -> [FilePath]
                   -> RIO env (Int, Int)
processSourceFiles templates paths = do
  Configuration {..} <- viewL
  year               <- viewL
  let dVars        = dynamicVariables year
      withTemplate = mapMaybe (template cLicenseHeaders) paths
  cVars     <- compileVariables (dVars <> cVariables)
  processed <- mapM (process cVars dVars) (zipWithProgress withTemplate)
  pure (L.length withTemplate, L.length . filter (== True) $ processed)
 where
  fileType c p = fileExtension p >>= fileTypeByExt c
  template c p = (, p) <$> (fileType c p >>= \ft -> M.lookup ft templates)
  process cVars dVars (pr, (ti, p)) = processSourceFile cVars dVars pr ti p


processSourceFile :: ( Has CommandRunOptions env
                     , Has CtConfiguration env
                     , Has CtHeaderFnConfigs env
                     , Has CurrentYear env
                     , HasLogFunc env
                     )
                  => Variables
                  -> Variables
                  -> Progress
                  -> TemplateInfo
                  -> FilePath
                  -> RIO env Bool
processSourceFile cVars dVars progress ti@TemplateInfo {..} path = do
  Configuration {..}     <- viewL @CtConfiguration
  CommandRunOptions {..} <- viewL
  fileContent            <- readFileUtf8 path
  let fileInfo@FileInfo {..} = extractFileInfo ti fileContent
      variables              = dVars <> cVars <> fiVariables
      syntax                 = hcHeaderSyntax fiHeaderConfig
  header'        <- renderTemplate variables tiTemplate
  header         <- postProcessHeader' syntax variables header'
  RunAction {..} <- chooseAction fileInfo header
  let result  = raFunc fileContent
      changed = raProcessed && (fileContent /= result)
      message = if changed then raProcessedMsg else raSkippedMsg
      isCheck = cRunMode == Check
  logDebug $ "File info: " <> displayShow fileInfo
  logInfo $ mconcat [display progress, " ", display message, fromString path]
  when (not croDryRun && not isCheck && changed) (writeFileUtf8 path result)
  pure changed


chooseAction :: (Has CtConfiguration env)
             => FileInfo
             -> Text
             -> RIO env RunAction
chooseAction info header = do
  Configuration {..} <- viewL @CtConfiguration
  let hasHeader = isJust $ fiHeaderPos info
  pure $ go cRunMode hasHeader
 where
  go runMode hasHeader = case runMode of
    Add     -> aAction hasHeader
    Check   -> cAction hasHeader
    Drop    -> dAction hasHeader
    Replace -> rAction hasHeader
  aAction hasHeader = RunAction (not hasHeader)
                                (addHeader info header)
                                (justify "Adding header to:")
                                (justify "Header already exists in:")
  cAction hasHeader = (rAction hasHeader)
    { raProcessedMsg = justify "Outdated header found in:"
    , raSkippedMsg   = justify "Header up-to-date in:"
    }
  dAction hasHeader = RunAction hasHeader
                                (dropHeader info)
                                (justify "Dropping header from:")
                                (justify "No header exists in:")
  rAction hasHeader = if hasHeader then rAction' else go Add hasHeader
  rAction' = RunAction True
                       (replaceHeader info header)
                       (justify "Replacing header in:")
                       (justify "Header up-to-date in:")
  justify = T.justifyLeft 30 ' '


-- | Loads templates from the given paths.
loadTemplateFiles :: (Has (FileSystem (RIO env)) env, HasLogFunc env)
                  => [FilePath]
                  -- ^ paths to template files
                  -> RIO env (Map FileType TemplateType)
                  -- ^ map of file types and templates
loadTemplateFiles paths' = do
  FileSystem {..} <- viewL
  paths           <- mconcat <$> mapM (`fsFindFilesByExts` extensions) paths'
  logDebug $ "Using template paths: " <> displayShow paths
  withTypes <- catMaybes <$> mapM (\p -> fmap (, p) <$> typeOfTemplate p) paths
  parsed    <- mapM
    (\(t, p) ->
      (t, ) <$> ((T.strip <$> fsLoadFile p) >>= parseTemplate (Just $ T.pack p))
    )
    withTypes
  logInfo
    $ mconcat ["Found ", display $ L.length parsed, " license template(s)"]
  pure $ M.fromList parsed
  where extensions = toList $ templateExtensions @TemplateType


-- | Loads built-in templates, stored in "Headroom.Embedded", for the given
-- 'LicenseType'.
loadBuiltInTemplates :: (HasLogFunc env)
                     => LicenseType
                     -- ^ license type for which to selected templates
                     -> RIO env (Map FileType TemplateType)
                     -- ^ map of file types and templates
loadBuiltInTemplates licenseType = do
  logInfo $ "Using built-in templates for license: " <> displayShow licenseType
  parsed <- mapM (\(t, r) -> (t, ) <$> parseTemplate Nothing r) rawTemplates
  pure $ M.fromList parsed
 where
  rawTemplates = fmap (\ft -> (ft, template ft)) (allValues @FileType)
  template     = licenseTemplate licenseType


loadTemplates :: ( Has CtConfiguration env
                 , Has (FileSystem (RIO env)) env
                 , HasLogFunc env
                 )
              => RIO env (Map FileType TemplateInfo)
loadTemplates = do
  Configuration {..} <- viewL @CtConfiguration
  templates          <- case cTemplateSource of
    TemplateFiles    paths       -> loadTemplateFiles paths
    BuiltInTemplates licenseType -> loadBuiltInTemplates licenseType
  pure $ M.mapWithKey (mkTemplateInfo cLicenseHeaders) templates


-- | Takes path to the template file and returns detected type of the template.
typeOfTemplate :: HasLogFunc env
               => FilePath
               -- ^ path to the template file
               -> RIO env (Maybe FileType)
               -- ^ detected template type
typeOfTemplate path = do
  let fileType = textToEnum . T.pack . takeBaseName $ path
  when (isNothing fileType)
       (logWarn $ "Skipping unrecognized template type: " <> fromString path)
  pure fileType


loadConfigurationSafe :: (HasLogFunc env)
                      => FilePath
                      -> RIO env (Maybe PtConfiguration)
loadConfigurationSafe path = catch (Just <$> loadConfiguration path) onError
 where
  onError err = do
    logDebug $ displayShow (err :: IOException)
    logInfo $ mconcat
      [ "Configuration file '"
      , fromString path
      , "' not found. You can either specify all required parameter by "
      , "command line arguments, or generate one using "
      , "'headroom gen -c >"
      , configFileName
      , "'. See official documentation "
      , "for more details."
      ]
    pure Nothing


finalConfiguration :: (HasLogFunc env, Has CommandRunOptions env)
                   => RIO env CtConfiguration
finalConfiguration = do
  logInfo $ display productInfo
  defaultConfig' <- Just <$> parseConfiguration defaultConfig
  cmdLineConfig  <- Just <$> optionsToConfiguration
  yamlConfig     <- loadConfigurationSafe configFileName
  let mergedConfig =
        mconcat . catMaybes $ [defaultConfig', yamlConfig, cmdLineConfig]
  config <- makeConfiguration mergedConfig
  logDebug $ "Default config: " <> displayShow defaultConfig'
  logDebug $ "YAML config: " <> displayShow yamlConfig
  logDebug $ "CmdLine config: " <> displayShow cmdLineConfig
  logDebug $ "Merged config: " <> displayShow mergedConfig
  logDebug $ "Final config: " <> displayShow config
  pure config


optionsToConfiguration :: (Has CommandRunOptions env) => RIO env PtConfiguration
optionsToConfiguration = do
  CommandRunOptions {..} <- viewL
  variables              <- parseVariables croVariables
  pure Configuration { cRunMode         = maybe mempty pure croRunMode
                     , cSourcePaths     = ifNot null croSourcePaths
                     , cExcludedPaths   = ifNot null croExcludedPaths
                     , cTemplateSource  = maybe mempty pure croTemplateSource
                     , cVariables       = variables
                     , cLicenseHeaders  = mempty
                     , cHeaderFnConfigs = mempty
                     }
  where ifNot cond value = if cond value then mempty else pure value


currentYear :: (MonadIO m) => m CurrentYear
currentYear = do
  now      <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow      = utcToLocalTime timezone now
      (year, _, _) = toGregorian $ localDay zoneNow
  pure $ CurrentYear year


-- | Performs post-processing on rendered /license header/, based on given
-- configuration. Currently the main points are to:
--
--  1. sanitize possibly corrupted comment syntax ('sanitizeHeader')
--  2. apply /license header functions/ ('postProcessHeader')
postProcessHeader' :: (Has CtHeaderFnConfigs env, Has CurrentYear env)
                   => HeaderSyntax
                   -- ^ syntax of the license header comments
                   -> Variables
                   -- ^ template variables
                   -> Text
                   -- ^ rendered /license header/ to post-process
                   -> RIO env Text
                   -- ^ post-processed /license header/
postProcessHeader' syntax vars rawHeader = do
  configs <- viewL @CtHeaderFnConfigs
  year    <- viewL
  cEnv    <- mkConfiguredEnv year vars configs
  let processed = sanitizeHeader syntax . postProcessHeader cEnv $ rawHeader
  pure processed


-- | Ensures that all lines in license header starts with /line-comment/ syntax
-- if such syntax is used for license header.
--
-- >>> sanitizeHeader (LineComment "--") "-- foo\nbar\n-- baz"
-- "-- foo\n-- bar\n-- baz"
sanitizeHeader :: HeaderSyntax
               -- ^ syntax of the license header comments
               -> Text
               -- ^ input text to sanitize
               -> Text
               -- ^ sanitized text
sanitizeHeader (BlockComment _ _      ) text = text
sanitizeHeader (LineComment prefixedBy) text = mapLines process text
 where
  process line | T.isPrefixOf prefixedBy line = line
               | otherwise                    = prefixedBy <> " " <> line
