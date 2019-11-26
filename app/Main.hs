{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Headroom.Main.App
import           Headroom.Main.CmdOptions       ( cmdOptions
                                                , toAppConfig
                                                )
import           Headroom.AppConfig             ( loadAppConfig )
import           Headroom.Types                 ( AppConfig )
import           RIO
import           RIO.Directory
import           RIO.FilePath                   ( (</>) )
import           System.Console.CmdArgs


main :: IO ()
main = runApp $ do
  logInfo "starting app..."
  appConfig <- mergedAppConfig
  displayArgs

runApp :: RIO App a -> IO a
runApp inner = do
  cmdOptions' <- cmdArgsRun cmdOptions
  logOptions  <- logOptionsHandle stderr False
  let logOptions' = setLogMinLevel LevelDebug logOptions
  withLogFunc logOptions' $ \logFunc -> do
    let app = App { appCmdOptions = cmdOptions', appLogFunc = logFunc }
    runRIO app inner

displayArgs :: (HasCmdOptions env, HasLogFunc env) => RIO env ()
displayArgs = do
  cmdOptions' <- view cmdOptionsL
  logInfo $ "parsed cmdargs: " <> displayShow cmdOptions'

mergedAppConfig :: (HasCmdOptions env, HasLogFunc env) => RIO env AppConfig
mergedAppConfig = do
  cmdOptions' <- view cmdOptionsL
  configDir   <- getXdgDirectory XdgConfig "headroom"
  currDir     <- getCurrentDirectory
  let locations = [currDir </> configFile, configDir </> configFile]
  logInfo
    $  "trying to load configuration from following paths: "
    <> displayShow locations
  appConfigs <- fmap catMaybes (mapM loadAppConfigSafe locations)
  mergeAppConfigs $ toAppConfig cmdOptions' : appConfigs
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
