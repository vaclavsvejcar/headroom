{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Command.Run
  ( commandRun
  )
where

import           Headroom.Command.Utils         ( bootstrap )
import           Headroom.Configuration         ( loadConfiguration
                                                , makeConfiguration
                                                , parseConfiguration
                                                , parseVariables
                                                )
import           Headroom.Embedded              ( defaultConfig )
import           Headroom.Types                 ( CommandRunOptions(..)
                                                , Configuration(..)
                                                , PartialConfiguration(..)
                                                , RunMode(..)
                                                )
import           RIO


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
  logInfo "TODO"

mergedConfiguration :: (HasLogFunc env, HasRunOptions env)
                    => RIO env Configuration
mergedConfiguration = do
  defaultConfig' <- parseConfiguration defaultConfig
  cmdLineConfig  <- optionsToConfiguration
  yamlConfig     <- loadConfiguration ".headroom.yaml"
  config <- makeConfiguration $ defaultConfig' <> yamlConfig <> cmdLineConfig
  logDebug $ "Default config: " <> displayShow defaultConfig'
  logDebug $ "YAML config: " <> displayShow yamlConfig
  logDebug $ "CMDLine config: " <> displayShow cmdLineConfig
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

