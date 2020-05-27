{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
  )
where

import           Data.Time.Calendar             ( toGregorian )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Data.Time.LocalTime            ( getCurrentTimeZone
                                                , localDay
                                                , utcToLocalTime
                                                )
import           Headroom.Command.Utils         ( bootstrap )
import           Headroom.Configuration         ( loadConfiguration
                                                , makeConfiguration
                                                , parseConfiguration
                                                )
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Data.Has              ( Has(..) )
import           Headroom.Embedded              ( defaultConfig
                                                , licenseTemplate
                                                )
import           Headroom.FileSupport           ( addHeader
                                                , dropHeader
                                                , extractFileInfo
                                                , replaceHeader
                                                )
import           Headroom.FileSystem            ( excludePaths
                                                , fileExtension
                                                , findFilesByExts
                                                , findFilesByTypes
                                                , loadFile
                                                )
import           Headroom.FileType              ( configByFileType
                                                , fileTypeByExt
                                                )
import           Headroom.Meta                  ( TemplateType
                                                , productInfo
                                                )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( CommandRunOptions(..)
                                                , Configuration(..)
                                                , CurrentYear(..)
                                                , FileInfo(..)
                                                , FileType(..)
                                                , LicenseType(..)
                                                , PartialConfiguration(..)
                                                , RunAction(..)
                                                , RunMode(..)
                                                , TemplateSource(..)
                                                , Variables(..)
                                                )
import           Headroom.UI                    ( Progress(..)
                                                , zipWithProgress
                                                )
import           Headroom.Variables             ( compileVariables
                                                , dynamicVariables
                                                , parseVariables
                                                )
import           RIO
import           RIO.FilePath                   ( takeBaseName )
import qualified RIO.List                      as L
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T



-- | Initial /RIO/ startup environment for the /Run/ command.
data StartupEnv = StartupEnv
  { envLogFunc    :: !LogFunc
  -- ^ logging function
  , envRunOptions :: !CommandRunOptions
  -- ^ options
  }

-- | Full /RIO/ environment for the /Run/ command.
data Env = Env
  { envEnv           :: !StartupEnv
  -- ^ startup /RIO/ environment
  , envConfiguration :: !Configuration
  -- ^ application configuration
  , envCurrentYear   :: !CurrentYear
  -- ^ current year
  }

instance Has Configuration Env where
  hasLens = lens envConfiguration (\x y -> x { envConfiguration = y })

instance Has StartupEnv StartupEnv where
  hasLens = id

instance Has StartupEnv Env where
  hasLens = lens envEnv (\x y -> x { envEnv = y })

instance HasLogFunc StartupEnv where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasLogFunc Env where
  logFuncL = hasLens @StartupEnv . logFuncL

instance Has CommandRunOptions StartupEnv where
  hasLens = lens envRunOptions (\x y -> x { envRunOptions = y })

instance Has CommandRunOptions Env where
  hasLens = hasLens @StartupEnv . hasLens

instance Has CurrentYear Env where
  hasLens = lens envCurrentYear (\x y -> x { envCurrentYear = y })


env' :: CommandRunOptions -> LogFunc -> IO Env
env' opts logFunc = do
  let envEnv = StartupEnv { envLogFunc = logFunc, envRunOptions = opts }
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
  Configuration {..}     <- viewL
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


findSourceFiles :: (Has Configuration env, HasLogFunc env)
                => [FileType]
                -> RIO env [FilePath]
findSourceFiles fileTypes = do
  Configuration {..} <- viewL
  logDebug $ "Using source paths: " <> displayShow cSourcePaths
  files <- mconcat <$> mapM (findFiles' cLicenseHeaders) cSourcePaths
  let files' = excludePaths cExcludedPaths files
  logInfo $ mconcat
    [ "Found "
    , display $ L.length files'
    , " source file(s) (excluded "
    , display $ L.length files - L.length files'
    , " file(s))"
    ]
  pure files'
  where findFiles' licenseHeaders = findFilesByTypes licenseHeaders fileTypes


processSourceFiles :: ( Has Configuration env
                      , Has CommandRunOptions env
                      , Has CurrentYear env
                      , HasLogFunc env
                      )
                   => Map FileType TemplateType
                   -> [FilePath]
                   -> RIO env (Int, Int)
processSourceFiles templates paths = do
  Configuration {..} <- viewL
  year               <- viewL
  cVars              <- compileVariables cVariables
  let dVars        = dynamicVariables year
      withFileType = mapMaybe (findFileType cLicenseHeaders) paths
      withTemplate = mapMaybe (uncurry findTemplate) withFileType
  processed <- mapM (process cVars dVars) (zipWithProgress withTemplate)
  pure (L.length withTemplate, L.length . filter (== True) $ processed)
 where
  findFileType conf path =
    fmap (, path) (fileExtension path >>= fileTypeByExt conf)
  findTemplate ft p = (, ft, p) <$> M.lookup ft templates
  process cVars dVars (pr, (tt, ft, p)) =
    processSourceFile cVars dVars pr tt ft p


processSourceFile :: ( Has Configuration env
                     , Has CommandRunOptions env
                     , Has CurrentYear env
                     , HasLogFunc env
                     )
                  => Variables
                  -> Variables
                  -> Progress
                  -> TemplateType
                  -> FileType
                  -> FilePath
                  -> RIO env Bool
processSourceFile cVars dVars progress template fileType path = do
  Configuration {..}     <- viewL
  CommandRunOptions {..} <- viewL
  year                   <- viewL
  fileContent            <- readFileUtf8 path
  let fileInfo = extractFileInfo fileType
                                 (configByFileType cLicenseHeaders fileType)
                                 year
                                 fileContent
      variables = dVars <> cVars <> fiVariables fileInfo
  header         <- renderTemplate variables template
  RunAction {..} <- chooseAction fileInfo header
  let result  = raFunc fileContent
      changed = raProcessed && (fileContent /= result)
      message = if changed then raProcessedMsg else raSkippedMsg
      isCheck = cRunMode == Check
  logDebug $ "File info: " <> displayShow fileInfo
  logInfo $ mconcat [display progress, " ", display message, fromString path]
  when (not croDryRun && not isCheck && changed) (writeFileUtf8 path result)
  pure changed


chooseAction :: (Has Configuration env) => FileInfo -> Text -> RIO env RunAction
chooseAction info header = do
  Configuration {..} <- viewL
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
loadTemplateFiles :: (HasLogFunc env)
                  => [FilePath]
                  -- ^ paths to template files
                  -> RIO env (Map FileType TemplateType)
                  -- ^ map of file types and templates
loadTemplateFiles paths' = do
  paths <- mconcat <$> mapM (`findFilesByExts` extensions) paths'
  logDebug $ "Using template paths: " <> displayShow paths
  withTypes <- catMaybes <$> mapM (\p -> fmap (, p) <$> typeOfTemplate p) paths
  parsed    <- mapM (\(t, p) -> (t, ) <$> load p) withTypes
  logInfo
    $ mconcat ["Found ", display $ L.length parsed, " license template(s)"]
  pure $ M.fromList parsed
 where
  extensions = toList $ templateExtensions @TemplateType
  load path =
    liftIO $ (T.strip <$> loadFile path) >>= parseTemplate (Just $ T.pack path)


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


loadTemplates :: (Has Configuration env, HasLogFunc env)
              => RIO env (Map FileType TemplateType)
loadTemplates = do
  Configuration {..} <- viewL
  case cTemplateSource of
    TemplateFiles    paths       -> loadTemplateFiles paths
    BuiltInTemplates licenseType -> loadBuiltInTemplates licenseType


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
                      -> RIO env (Maybe PartialConfiguration)
loadConfigurationSafe path = catch (Just <$> loadConfiguration path) onError
 where
  onError err = do
    logDebug $ displayShow (err :: IOException)
    logInfo $ mconcat
      [ "Configuration file '"
      , fromString path
      , "' not found. You can either specify all required parameter by "
      , "command line arguments, or generate one using "
      , "'headroom gen -c >.headroom.yaml'. See official documentation "
      , "for more details."
      ]
    pure Nothing


finalConfiguration :: (HasLogFunc env, Has CommandRunOptions env)
                   => RIO env Configuration
finalConfiguration = do
  logInfo $ display productInfo
  defaultConfig' <- Just <$> parseConfiguration defaultConfig
  cmdLineConfig  <- Just <$> optionsToConfiguration
  yamlConfig     <- loadConfigurationSafe ".headroom.yaml"
  let mergedConfig =
        mconcat . catMaybes $ [defaultConfig', yamlConfig, cmdLineConfig]
  config <- makeConfiguration mergedConfig
  logDebug $ "Default config: " <> displayShow defaultConfig'
  logDebug $ "YAML config: " <> displayShow yamlConfig
  logDebug $ "CmdLine config: " <> displayShow cmdLineConfig
  logDebug $ "Merged config: " <> displayShow mergedConfig
  logDebug $ "Final config: " <> displayShow config
  pure config


optionsToConfiguration :: (Has CommandRunOptions env)
                       => RIO env PartialConfiguration
optionsToConfiguration = do
  CommandRunOptions {..} <- viewL
  variables              <- parseVariables croVariables
  pure PartialConfiguration
    { pcRunMode        = maybe mempty pure croRunMode
    , pcSourcePaths    = ifNot null croSourcePaths
    , pcExcludedPaths  = ifNot null croExcludedPaths
    , pcTemplateSource = maybe mempty pure croTemplateSource
    , pcVariables      = variables
    , pcLicenseHeaders = mempty
    }
  where ifNot cond value = if cond value then mempty else pure value


currentYear :: (MonadIO m) => m CurrentYear
currentYear = do
  now      <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow      = utcToLocalTime timezone now
      (year, _, _) = toGregorian $ localDay zoneNow
  pure $ CurrentYear year
