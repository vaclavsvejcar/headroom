{-|
Module      : Headroom.Command.Run
Description : Logic for Run command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Logic for the @run@ command, used to add or replace license headers in source
code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Headroom.Command.Run
  ( commandRun
  )
where

import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Headroom.AppConfig             ( AppConfig(..)
                                                , loadAppConfig
                                                , validateAppConfig
                                                )
import           Headroom.Command.Run.Env
import           Headroom.Command.Shared        ( bootstrap )
import           Headroom.FileSystem            ( fileExtension
                                                , findFilesByExts
                                                , findFilesByTypes
                                                )
import           Headroom.FileType              ( FileType
                                                , fileTypeByExt
                                                , fileTypeByName
                                                )
import           Headroom.Global                ( TemplateType )
import           Headroom.Header                ( Header(..)
                                                , addHeader
                                                , containsHeader
                                                , dropHeader
                                                , replaceHeader
                                                )
import           Headroom.Template              ( Template(..)
                                                , loadTemplate
                                                )
import           Headroom.Types                 ( RunMode(..) )
import           Headroom.UI.Progress           ( Progress(..) )
import           RIO                     hiding ( second )
import           RIO.Directory
import           RIO.FilePath                   ( takeBaseName
                                                , (</>)
                                                )
import qualified RIO.List                      as L
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T


env' :: RunOptions -> LogFunc -> IO Env
env' opts logFunc = do
  let startupEnv = StartupEnv { envLogFunc = logFunc, envRunOptions = opts }
  merged <- runRIO startupEnv mergedAppConfig
  pure $ Env { envEnv = startupEnv, envAppConfig = merged }

-- | Handler for /Run/ command.
commandRun :: RunOptions -- ^ /Run/ command options
           -> IO ()      -- ^ execution result
commandRun opts = bootstrap (env' opts) (roDebug opts) $ do
  startTS <- liftIO getPOSIXTime
  logInfo "Loading source code header templates..."
  templates <- loadTemplates
  logInfo $ "Done, found " <> display (M.size templates) <> " template(s)"
  logInfo "Searching for source code files..."
  sourceFiles <- findSourceFiles (M.keys templates)
  let sourceFilesNum = display . L.length $ sourceFiles
  logInfo $ mconcat
    ["Done, found ", sourceFilesNum, " sources code files(s) to process"]
  (total, skipped) <- processHeaders templates sourceFiles
  endTS            <- liftIO getPOSIXTime
  let (elapsedSeconds, _) = properFraction (endTS - startTS)
  logInfo $ mconcat
    [ "Done: modified "
    , display (total - skipped)
    , ", skipped "
    , display skipped
    , " files in "
    , display (elapsedSeconds :: Integer)
    , " second(s)."
    ]

mergedAppConfig :: (HasRunOptions env, HasLogFunc env) => RIO env AppConfig
mergedAppConfig = do
  runOptions <- view runOptionsL
  configDir  <- getXdgDirectory XdgConfig "headroom"
  currDir    <- getCurrentDirectory
  let locations = [currDir </> ".headroom.yaml", configDir </> "headroom.yaml"]
  logInfo "Loading configuration file(s)..."
  logDebug $ "Configuration files locations: " <> displayShow locations
  appConfigs        <- fmap catMaybes (mapM loadAppConfigSafe locations)
  appConfigFromOpts <- toAppConfig runOptions
  merged            <- mergeAppConfigs $ appConfigFromOpts : appConfigs
  validateAppConfig merged
 where
  loadAppConfigSafe path = catch
    (fmap Just (loadAppConfig path))
    (\ex -> do
      logDebug $ displayShow (ex :: IOException)
      logWarn $ "Skipping missing configuration file: " <> fromString path
      pure Nothing
    )
  mergeAppConfigs appConfigs = do
    let merged = mconcat appConfigs
    logDebug $ "Source AppConfig instances: " <> displayShow appConfigs
    logDebug $ "Merged AppConfig: " <> displayShow merged
    pure merged

loadTemplates :: (HasAppConfig env, HasLogFunc env)
              => RIO env (M.Map FileType Text)
loadTemplates = do
  appConfig <- view appConfigL
  paths     <- liftIO (mconcat <$> mapM findPaths (acTemplatePaths appConfig))
  logDebug $ "Found template files: " <> displayShow paths
  withTypes <- mapM (\path -> fmap (, path) (extractTemplateType path)) paths
  parsed    <- mapM (\(t, p) -> fmap (t, ) (loadTemplate p))
                    (mapMaybe filterTemplate withTypes)
  rendered <- mapM
    (\(t, p) ->
      fmap (t, ) (renderTemplate (acVariables appConfig) (p :: TemplateType))
    )
    parsed
  pure $ M.fromList rendered
 where
  extensions = templateExtensions (Proxy :: Proxy TemplateType)
  findPaths path = findFilesByExts path extensions
  filterTemplate (fileType, path) = (\ft -> Just (ft, path)) =<< fileType

extractTemplateType :: HasLogFunc env => FilePath -> RIO env (Maybe FileType)
extractTemplateType path = do
  let fileType = fileTypeByName . T.pack . takeBaseName $ path
  when (isNothing fileType)
       (logWarn $ "Skipping unrecognized template type: " <> fromString path)
  pure fileType

findSourceFiles :: HasAppConfig env => [FileType] -> RIO env [FilePath]
findSourceFiles fileTypes = do
  appConfig <- view appConfigL
  let paths = acSourcePaths appConfig
  liftIO $ fmap concat (mapM (`findFilesByTypes` fileTypes) paths)

processHeaders :: (HasLogFunc env, HasRunOptions env)
               => M.Map FileType Text
               -> [FilePath]
               -> RIO env (Int, Int)
processHeaders templates paths = do
  let filesToProcess = mapMaybe withTemplate (mapMaybe processPath paths)
      zipped         = L.zip [1 ..] filesToProcess
      withProgress   = fmap (\(i, (h, p)) -> (progress i, h, p)) zipped
      progress curr = Progress curr (L.length paths)
  processed <- mapM (\(i, h, p) -> processHeader i h p) withProgress
  pure (L.length withProgress, L.length . filter (== True) $ processed)
 where
  withTemplate (fileType, path) =
    fmap (\t -> (Header fileType t, path)) (M.lookup fileType templates)
  processPath path = fmap (, path) (fileExtension path >>= fileTypeByExt)

processHeader :: (HasLogFunc env, HasRunOptions env)
              => Progress
              -> Header
              -> FilePath
              -> RIO env Bool
processHeader progress header path = do
  runOptions  <- view runOptionsL
  fileContent <- readFileUtf8 path
  let hasHeader              = containsHeader (hFileType header) fileContent
      (skipped, action, msg) = chooseAction (roRunMode runOptions) hasHeader
      msg'                   = if skipped then "Skipping file" else msg
  log' $ msg' <> ": " <> fromString path
  writeFileUtf8 path (action header fileContent)
  pure skipped
 where
  log' msg = logInfo $ display progress <> " " <> msg
  chooseAction runMode hasHeader = case runMode of
    Add     -> (hasHeader, addHeader, "Adding header to")
    Drop    -> (not hasHeader, dropHeader . hFileType, "Dropping header from")
    Replace -> if hasHeader
      then (False, replaceHeader, "Replacing header in")
      else chooseAction Add hasHeader
