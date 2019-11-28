{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Headroom.Run
  ( runMode
  )
where

import           Control.Applicative            ( (<$>) )
import           Data.Tuple.Extra               ( second )
import           Headroom.AppConfig             ( loadAppConfig )
import           Headroom.Filesystem            ( findFilesByExts )
import           Headroom.FileType              ( parseFileType )
import           Headroom.Run.Env
import           Headroom.Template              ( loadTemplate
                                                , renderTemplate
                                                )
import           Headroom.Types                 ( AppConfig(..)
                                                , FileType
                                                )
import           RIO                     hiding ( second )
import           RIO.Directory
import           RIO.FilePath                   ( (</>)
                                                , takeBaseName
                                                )
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T

runMode :: RunOptions -> IO ()
runMode opts = bootstrap opts $ do
  templates <- loadTemplates
  logInfo $ "Done loading " <> displayShow (M.size templates) <> " template(s)"
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
    $  "Trying to load configuration from following paths: "
    <> displayShow locations
  appConfigs <- fmap catMaybes (mapM loadAppConfigSafe locations)
  mergeAppConfigs $ toAppConfig runOptions : appConfigs
 where
  configFile = ".headroom.yaml"
  loadAppConfigSafe path = catch
    (fmap Just (loadAppConfig path))
    (\ex -> do
      logDebug $ displayShow (ex :: IOException)
      logWarn $ "Skipping missing configuration file: " <> fromString path
      return Nothing
    )
  mergeAppConfigs appConfigs = do
    let merged = mconcat appConfigs
    logDebug $ "Source AppConfig instances: " <> displayShow appConfigs
    logDebug $ "Merged AppConfig: " <> displayShow merged
    return merged

loadTemplates
  :: (HasAppConfig env, HasLogFunc env) => RIO env (M.Map FileType T.Text)
loadTemplates = do
  appConfig <- view appConfigL
  paths     <- liftIO (mconcat <$> mapM findPaths (acTemplatePaths appConfig))
  logDebug $ "Found template files: " <> displayShow paths
  withTypes <- mapM (\path -> fmap (, path) (extractTemplateType path)) paths
  let knownTemplates = mapMaybe filterTemplate withTypes
  parsed <- mapM (\(t, p) -> fmap (t, ) (loadTemplate p)) knownTemplates
  return $ M.fromList
    (fmap (second $ renderTemplate $ acPlaceholders appConfig) parsed)
 where
  findPaths path = findFilesByExts path ["jinja", "jinja2"]
  filterTemplate pair = case pair of
    (Just fileType, path) -> Just (fileType, path)
    _                     -> Nothing

extractTemplateType :: HasLogFunc env => FilePath -> RIO env (Maybe FileType)
extractTemplateType path = do
  let fileType = parseFileType . T.pack . takeBaseName $ path
  when (isNothing fileType)
       (logWarn $ "Skipping unrecognized template type: " <> fromString path)
  return fileType
