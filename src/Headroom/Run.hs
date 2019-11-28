{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Run
  ( runMode
  )
where

import           Control.Applicative            ( (<$>) )
import           Headroom.AppConfig             ( loadAppConfig )
import           Headroom.Filesystem            ( findFilesByExts )
import           Headroom.Run.Env
import           Headroom.Types                 ( AppConfig(..)
                                                , FileType
                                                )
import           RIO
import           RIO.Directory
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

runMode :: RunOptions -> IO ()
runMode opts = bootstrap opts $ do
  logInfo "todo"
  templates <- loadTemplates
  return ()

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

loadTemplates
  :: (HasAppConfig env, HasLogFunc env) => RIO env (HM.HashMap FileType T.Text)
loadTemplates = do
  appConfig <- view appConfigL
  paths     <- liftIO (mconcat <$> mapM findPaths (acTemplatePaths appConfig))
  logDebug $ "template paths: " <> displayShow paths
  return HM.empty   -- TODO logic
  where findPaths path = findFilesByExts path ["jinja", "jinja2"]
