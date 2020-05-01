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
  )
where

import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Headroom.Command.Utils         ( bootstrap )
import           Headroom.Configuration         ( loadConfiguration
                                                , makeConfiguration
                                                , parseConfiguration
                                                , parseVariables
                                                )
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Data.Has              ( Has(..) )
import           Headroom.Embedded              ( defaultConfig )
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
                                                , FileInfo(..)
                                                , FileType(..)
                                                , PartialConfiguration(..)
                                                , RunMode(..)
                                                )
import           Headroom.UI                    ( Progress(..)
                                                , zipWithProgress
                                                )
import           RIO
import           RIO.FilePath                   ( takeBaseName )
import qualified RIO.List                      as L
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T



-- | Initial /RIO/ startup environment for the /Run/ command.
data StartupEnv = StartupEnv
  { envLogFunc    :: !LogFunc           -- ^ logging function
  , envRunOptions :: !CommandRunOptions -- ^ options
  }

-- | Full /RIO/ environment for the /Run/ command.
data Env = Env
  { envEnv           :: !StartupEnv     -- ^ startup /RIO/ environment
  , envConfiguration :: !Configuration  -- ^ application configuration
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


env' :: CommandRunOptions -> LogFunc -> IO Env
env' opts logFunc = do
  let startupEnv = StartupEnv { envLogFunc = logFunc, envRunOptions = opts }
  merged <- runRIO startupEnv finalConfiguration
  pure $ Env { envEnv = startupEnv, envConfiguration = merged }


-- | Handler for /Run/ command.
commandRun :: CommandRunOptions -- ^ /Run/ command options
           -> IO ()             -- ^ execution result
commandRun opts = bootstrap (env' opts) (croDebug opts) $ do
  logInfo $ display productInfo
  warnOnDryRun
  startTS            <- liftIO getPOSIXTime
  templates          <- loadTemplates
  sourceFiles        <- findSourceFiles (M.keys templates)
  (total, processed) <- processSourceFiles templates sourceFiles
  endTS              <- liftIO getPOSIXTime
  logInfo "-----"
  logInfo $ mconcat
    [ "Done: modified "
    , display processed
    , ", skipped "
    , display (total - processed)
    , " file(s) in "
    , displayShow (endTS - startTS)
    , " second(s)."
    ]
  warnOnDryRun

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
  logInfo $ mconcat ["Found ", display $ L.length files', " source file(s)"]
  pure files'
  where findFiles' licenseHeaders = findFilesByTypes licenseHeaders fileTypes


processSourceFiles :: ( Has Configuration env
                      , HasLogFunc env
                      , Has CommandRunOptions env
                      )
                   => Map FileType TemplateType
                   -> [FilePath]
                   -> RIO env (Int, Int)
processSourceFiles templates paths = do
  Configuration {..} <- viewL
  let withFileType = mapMaybe (findFileType cLicenseHeaders) paths
      withTemplate = mapMaybe (uncurry findTemplate) withFileType
  processed <- mapM process (zipWithProgress withTemplate)
  logDebug "foo"
  pure (L.length withTemplate, L.length . filter (== True) $ processed)
 where
  findFileType conf path =
    fmap (, path) (fileExtension path >>= fileTypeByExt conf)
  findTemplate ft p = (, ft, p) <$> M.lookup ft templates
  process (pr, (tt, ft, p)) = processSourceFile pr tt ft p


processSourceFile :: ( Has Configuration env
                     , HasLogFunc env
                     , Has CommandRunOptions env
                     )
                  => Progress
                  -> TemplateType
                  -> FileType
                  -> FilePath
                  -> RIO env Bool
processSourceFile progress template fileType path = do
  Configuration {..}     <- viewL
  CommandRunOptions {..} <- viewL
  fileContent            <- readFileUtf8 path
  let fileInfo = extractFileInfo fileType
                                 (configByFileType cLicenseHeaders fileType)
                                 fileContent
      variables = cVariables <> fiVariables fileInfo
  header                        <- renderTemplate variables template
  (processed, action, message') <- chooseAction fileInfo header
  let result  = action fileContent
      changed = processed && (fileContent /= result)
      message = if changed then message' else "Skipping file:        "
  logDebug $ "File info: " <> displayShow fileInfo
  logInfo $ mconcat [display progress, " ", display message, fromString path]
  when (not croDryRun && changed) (writeFileUtf8 path result)
  pure changed


chooseAction :: (Has Configuration env)
             => FileInfo
             -> Text
             -> RIO env (Bool, Text -> Text, Text)
chooseAction info header = do
  Configuration {..} <- viewL
  let hasHeader = isJust $ fiHeaderPos info
  pure $ go cRunMode hasHeader
 where
  go runMode hasHeader = case runMode of
    Add     -> (not hasHeader, addHeader info header, "Adding header to:     ")
    Drop    -> (hasHeader, dropHeader info, "Dropping header from: ")
    Replace -> if hasHeader
      then (True, replaceHeader info header, "Replacing header in:  ")
      else go Add hasHeader


loadTemplates :: (Has Configuration env, HasLogFunc env)
              => RIO env (Map FileType TemplateType)
loadTemplates = do
  Configuration {..} <- viewL
  paths <- mconcat <$> mapM (`findFilesByExts` extensions) cTemplatePaths
  logDebug $ "Using template paths: " <> displayShow paths
  withTypes <- catMaybes <$> mapM (\p -> fmap (, p) <$> typeOfTemplate p) paths
  parsed    <- mapM (\(t, p) -> (t, ) <$> load p) withTypes
  logInfo $ mconcat ["Found ", display $ L.length parsed, " template(s)"]
  pure $ M.fromList parsed
 where
  extensions = toList $ templateExtensions @TemplateType
  load path =
    liftIO $ (T.strip <$> loadFile path) >>= parseTemplate (Just $ T.pack path)


typeOfTemplate :: HasLogFunc env => FilePath -> RIO env (Maybe FileType)
typeOfTemplate path = do
  let fileType = textToEnum . T.pack . takeBaseName $ path
  when (isNothing fileType)
       (logWarn $ "Skipping unrecognized template type: " <> fromString path)
  pure fileType


finalConfiguration :: (HasLogFunc env, Has CommandRunOptions env)
                   => RIO env Configuration
finalConfiguration = do
  defaultConfig' <- parseConfiguration defaultConfig
  cmdLineConfig  <- optionsToConfiguration
  yamlConfig     <- loadConfiguration ".headroom.yaml"
  let mergedConfig = defaultConfig' <> yamlConfig <> cmdLineConfig
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
  runOptions <- viewL
  variables  <- parseVariables $ croVariables runOptions
  pure PartialConfiguration
    { pcRunMode        = maybe mempty pure (croRunMode runOptions)
    , pcSourcePaths    = ifNot null (croSourcePaths runOptions)
    , pcExcludedPaths  = ifNot null (croExcludedPaths runOptions)
    , pcTemplatePaths  = ifNot null (croTemplatePaths runOptions)
    , pcVariables      = ifNot null variables
    , pcLicenseHeaders = mempty
    }
  where ifNot cond value = if cond value then mempty else pure value
