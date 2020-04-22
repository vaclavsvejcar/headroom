{-|
Module      : Headroom.Command.Run
Description : Handler for the @run@ command.
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module representing the @run@ command, the core command of /Headroom/, which is
responsible for license header management.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
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
import           Headroom.Embedded              ( defaultConfig )
import           Headroom.FileSupport           ( addHeader
                                                , dropHeader
                                                , extractFileInfo
                                                , replaceHeader
                                                )
import           Headroom.FileSystem            ( fileExtension
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
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
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

class HasConfiguration env where
  configurationL :: Lens' env Configuration

-- | Environment value with /Init/ command options.
class HasRunOptions env where
  runOptionsL :: Lens' env CommandRunOptions

class (HasLogFunc env, HasRunOptions env) => HasEnv env where
  envL :: Lens' env StartupEnv

instance HasConfiguration Env where
  configurationL = lens envConfiguration (\x y -> x { envConfiguration = y })

instance HasEnv StartupEnv where
  envL = id

instance HasEnv Env where
  envL = lens envEnv (\x y -> x { envEnv = y })

instance HasLogFunc StartupEnv where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasLogFunc Env where
  logFuncL = envL . logFuncL

instance HasRunOptions StartupEnv where
  runOptionsL = lens envRunOptions (\x y -> x { envRunOptions = y })

instance HasRunOptions Env where
  runOptionsL = envL . runOptionsL


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


findSourceFiles :: (HasConfiguration env, HasLogFunc env)
                => [FileType]
                -> RIO env [FilePath]
findSourceFiles fileTypes = do
  Configuration {..} <- view configurationL
  logDebug $ "Using source paths: " <> displayShow cSourcePaths
  files <-
    mconcat <$> mapM (findFilesByTypes cLicenseHeaders fileTypes) cSourcePaths
  logInfo $ mconcat ["Found ", display $ L.length files, " source file(s)"]
  pure files


processSourceFiles :: (HasConfiguration env, HasLogFunc env)
                   => Map FileType TemplateType
                   -> [FilePath]
                   -> RIO env (Int, Int)
processSourceFiles templates paths = do
  Configuration {..} <- view configurationL
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


processSourceFile :: (HasConfiguration env, HasLogFunc env)
                  => Progress
                  -> TemplateType
                  -> FileType
                  -> FilePath
                  -> RIO env Bool
processSourceFile progress template fileType path = do
  Configuration {..} <- view configurationL
  fileContent        <- readFileUtf8 path
  let fileInfo = extractFileInfo fileType
                                 (configByFileType cLicenseHeaders fileType)
                                 fileContent
      variables = cVariables <> fiVariables fileInfo
  header                        <- renderTemplate variables template
  (processed, action, message') <- chooseAction fileInfo header
  let message = if processed then message' else "Skipping file:        "
  logDebug $ "File info: " <> displayShow fileInfo
  logInfo $ mconcat [display progress, " ", display message, fromString path]
  -- writeFileUtf8 path (action fileContent)
  pure processed


chooseAction :: (HasConfiguration env)
             => FileInfo
             -> Text
             -> RIO env (Bool, Text -> Text, Text)
chooseAction info header = do
  Configuration {..} <- view configurationL
  let hasHeader = isJust $ fiHeaderPos info
  pure $ go cRunMode hasHeader
 where
  go runMode hasHeader = case runMode of
    Add     -> (not hasHeader, addHeader info header, "Adding header to:     ")
    Drop    -> (hasHeader, dropHeader info, "Dropping header from: ")
    Replace -> if hasHeader
      then (True, replaceHeader info header, "Replacing header in:  ")
      else go Add hasHeader


loadTemplates :: (HasConfiguration env, HasLogFunc env)
              => RIO env (Map FileType TemplateType)
loadTemplates = do
  Configuration {..} <- view configurationL
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


finalConfiguration :: (HasLogFunc env, HasRunOptions env)
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


optionsToConfiguration :: (HasRunOptions env) => RIO env PartialConfiguration
optionsToConfiguration = do
  runOptions <- view runOptionsL
  variables  <- parseVariables $ croVariables runOptions
  pure PartialConfiguration
    { pcRunMode        = maybe mempty pure (croRunMode runOptions)
    , pcSourcePaths    = ifNot null (croSourcePaths runOptions)
    , pcTemplatePaths  = ifNot null (croTemplatePaths runOptions)
    , pcVariables      = ifNot null variables
    , pcLicenseHeaders = mempty
    }
  where ifNot cond value = if cond value then mempty else pure value
