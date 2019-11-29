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
import           Headroom.Filesystem            ( findFilesByExts
                                                , findFilesByTypes
                                                )
import           Headroom.FileType              ( fileTypeByExt
                                                , readFileType
                                                )
import           Headroom.Header                ( addHeader
                                                , replaceHeader
                                                )
import           Headroom.Run.Env
import           Headroom.Template              ( loadTemplate
                                                , renderTemplate
                                                )
import           Headroom.Types                 ( AppConfig(..)
                                                , FileType
                                                , Header(Header)
                                                )
import           RIO                     hiding ( second )
import           RIO.Directory
import           RIO.FilePath                   ( (</>)
                                                , takeBaseName
                                                , takeExtension
                                                )
import qualified RIO.List                      as L
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T

runMode :: RunOptions -> IO ()
runMode opts = bootstrap opts $ do
  templates <- loadTemplates
  logInfo $ "Done loading " <> displayShow (M.size templates) <> " template(s)"
  sourceFiles <- findSourceFiles (M.keys templates)
  let sourceFilesNum = displayShow . L.length $ sourceFiles
  logInfo $ "Found " <> sourceFilesNum <> " sources code files to process"
  processHeaders templates sourceFiles

bootstrap :: RunOptions -> RIO Env a -> IO a
bootstrap opts logic = do
  logOptions <- logOptionsHandle stderr (roDebug opts)
  let logOptions' = setLogUseLoc False logOptions
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
  let locations = [currDir </> ".headroom.yaml", configDir </> "headroom.yaml"]
  logInfo
    $  "Trying to load configuration from following paths: "
    <> displayShow locations
  appConfigs <- fmap catMaybes (mapM loadAppConfigSafe locations)
  mergeAppConfigs $ toAppConfig runOptions : appConfigs
 where
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
  parsed    <- mapM (\(t, p) -> fmap (t, ) (loadTemplate p))
                    (mapMaybe filterTemplate withTypes)
  return $ M.fromList
    (fmap (second $ renderTemplate $ acPlaceholders appConfig) parsed)
 where
  findPaths path = findFilesByExts path ["jinja", "jinja2"]
  filterTemplate (fileType, path) = (\ft -> Just (ft, path)) =<< fileType

extractTemplateType :: HasLogFunc env => FilePath -> RIO env (Maybe FileType)
extractTemplateType path = do
  let fileType = readFileType . T.pack . takeBaseName $ path
  when (isNothing fileType)
       (logWarn $ "Skipping unrecognized template type: " <> fromString path)
  return fileType

findSourceFiles :: HasRunOptions env => [FileType] -> RIO env [FilePath]
findSourceFiles fileTypes = do
  runOptions <- view runOptionsL
  let paths = roSourcePaths runOptions
  liftIO $ fmap concat (mapM (`findFilesByTypes` fileTypes) paths)

processHeaders
  :: (HasLogFunc env, HasRunOptions env)
  => M.Map FileType T.Text
  -> [FilePath]
  -> RIO env ()
processHeaders templates paths = do
  let filesToProcess = mapMaybe withTemplate (mapMaybe processPath paths)
  mapM_ (uncurry processHeader) filesToProcess
 where
  withTemplate (fileType, path) =
    fmap (\t -> (Header fileType t, path)) (M.lookup fileType templates)
  processPath path = fmap (, path) (fileTypeFor path)
  fileTypeFor = fileTypeByExt . T.pack . fileExt
  fileExt path = case takeExtension path of
    '.' : xs -> xs
    other    -> other

processHeader
  :: (HasLogFunc env, HasRunOptions env) => Header -> FilePath -> RIO env ()
processHeader header path = do
  logInfo $ "Processing file: " <> fromString path
  runOptions  <- view runOptionsL
  fileContent <- readFileUtf8 path
  let fn = if roReplaceHeaders runOptions then replaceHeader else addHeader
      processed = fn header fileContent
  writeFileUtf8 path processed
