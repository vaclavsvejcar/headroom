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
import           Headroom.FileSystem            ( findFilesByExts
                                                , findFilesByTypes
                                                , loadFile
                                                )
import           Headroom.Meta                  ( TemplateType )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( CommandRunOptions(..)
                                                , Configuration(..)
                                                , FileType(..)
                                                , PartialConfiguration(..)
                                                , RunMode(..)
                                                )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
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
  merged <- runRIO startupEnv mergedConfiguration
  pure $ Env { envEnv = startupEnv, envConfiguration = merged }

-- | Handler for /Run/ command.
commandRun :: CommandRunOptions -- ^ /Run/ command options
           -> IO ()             -- ^ execution result
commandRun opts = bootstrap (env' opts) (croDebug opts) $ do
  startTS     <- liftIO getPOSIXTime
  templates   <- loadTemplates
  sourceFiles <- findSourceFiles (M.keys templates)
  endTS       <- liftIO getPOSIXTime
  let (elapsedSeconds, _) = properFraction (endTS - startTS)
  logInfo
    $ mconcat ["Finished in ", display (elapsedSeconds :: Int), " second(s)"]

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
  load path = liftIO $ loadFile path >>= parseTemplate (Just $ T.pack path)

typeOfTemplate :: HasLogFunc env => FilePath -> RIO env (Maybe FileType)
typeOfTemplate path = do
  let fileType = textToEnum . T.pack . takeBaseName $ path
  when (isNothing fileType)
       (logWarn $ "Skipping unrecognized template type: " <> fromString path)
  pure fileType

mergedConfiguration :: (HasLogFunc env, HasRunOptions env)
                    => RIO env Configuration
mergedConfiguration = do
  defaultConfig' <- parseConfiguration defaultConfig
  cmdLineConfig  <- optionsToConfiguration
  yamlConfig     <- loadConfiguration ".headroom.yaml"
  config <- makeConfiguration $ defaultConfig' <> yamlConfig <> cmdLineConfig
  logDebug $ "Default config: " <> displayShow defaultConfig'
  logDebug $ "YAML config: " <> displayShow yamlConfig
  logDebug $ "CmdLine config: " <> displayShow cmdLineConfig
  logDebug $ "Merged config: " <> displayShow config
  pure config

optionsToConfiguration :: (HasRunOptions env) => RIO env PartialConfiguration
optionsToConfiguration = do
  runOptions <- view runOptionsL
  variables  <- parseVariables $ croVariables runOptions
  pure PartialConfiguration
    { pcRunMode        = ifNot (== Add) (croRunMode runOptions)
    , pcSourcePaths    = ifNot null (croSourcePaths runOptions)
    , pcTemplatePaths  = ifNot null (croTemplatePaths runOptions)
    , pcVariables      = ifNot null variables
    , pcLicenseHeaders = mempty
    }
  where ifNot cond value = if cond value then mempty else pure value

