{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Run
  ( runMode
  )
where

import           Headroom.AppConfig             ( loadAppConfig )
import           Headroom.Run.Env
import           Headroom.Types                 ( AppConfig )
import           RIO
import           RIO.Directory
import           RIO.FilePath                   ( (</>) )

runMode :: RunOptions -> IO ()
runMode opts = bootstrap opts $ do
  logInfo "todo"
  logInfo "todo"

bootstrap :: RunOptions -> RIO Env a -> IO a
bootstrap opts logic = do
  logOptions <- logOptionsHandle stderr False
  let logOptions' = setLogMinLevel LevelDebug logOptions
  withLogFunc logOptions' $ \logFunc -> do
    let startupEnv = StartupEnv { envLogFunc = logFunc, envRunOptions = opts }
    merged <- runRIO startupEnv mergedAppConfig
    let env = Env { envEnv = startupEnv, envAppConfig = merged }
    runRIO env logic

mergedAppConfig :: (HasRunOptions env, HasLogFunc env) => RIO env AppConfig
mergedAppConfig = do
  runOptions <- view runOptionsL
  configDir  <- getXdgDirectory XdgConfig "headroom"
  currDir    <- getCurrentDirectory
  let locations = [currDir </> configFile, configDir </> configFile]
  logInfo
    $  "trying to load configuration from following paths: "
    <> displayShow locations
  appConfigs <- fmap catMaybes (mapM loadAppConfigSafe locations)
  mergeAppConfigs $ toAppConfig runOptions : appConfigs
 where
  configFile = ".headroom.yaml"
  loadAppConfigSafe path = catch
    (fmap Just (loadAppConfig path))
    (\ex -> do
      logDebug $ displayShow (ex :: IOException)
      logWarn $ "skipping missing configuration file: " <> fromString path
      return Nothing
    )
  mergeAppConfigs appConfigs = do
    let merged = mconcat appConfigs
    logDebug $ "source AppConfig instances: " <> displayShow appConfigs
    logDebug $ "merged AppConfig: " <> displayShow merged
    return merged
