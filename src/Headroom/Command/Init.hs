{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Command.Init where

import           Headroom.Command.Init.Env
import           Headroom.Command.Shared        ( bootstrap )
import           Headroom.FileSystem            ( fileExtension
                                                , findFiles
                                                )
import           Headroom.FileType              ( FileType
                                                , fileTypeByExt
                                                )
import           RIO
import           RIO.Directory                  ( doesFileExist
                                                , getCurrentDirectory
                                                , createDirectory
                                                )
import           RIO.FilePath                   ( (</>) )
import qualified RIO.List                      as L


env' :: InitOptions -> LogFunc -> IO Env
env' opts logFunc = pure $ Env { envLogFunc = logFunc, envInitOptions = opts }

commandInit :: InitOptions -> IO ()
commandInit opts = bootstrap (env' opts) False $ do
  currentDir <- getCurrentDirectory
  doesAppConfigExist currentDir >>= \case
    False -> do
      logInfo "Searching supported file types to generate templates for..."
      fileTypes <- findSupportedFileTypes (ioSourcePaths opts)
      logInfo $ "Done, found supported file types: " <> displayShow fileTypes
      logInfo "Creating directory for templates 'headroom-templates'..."
      makeTemplatesDir currentDir
    True -> do
      logError "Config file '.headroom.yaml' already exists, exiting..."
      exitFailure

doesAppConfigExist :: MonadIO m => FilePath -> m Bool
doesAppConfigExist dirPath = doesFileExist $ dirPath </> ".headroom.yaml"

findSupportedFileTypes :: MonadIO m => [FilePath] -> m [FileType]
findSupportedFileTypes paths = do
  allFiles <- mapM (\path -> findFiles path (const True)) paths
  let allFileTypes = fmap (fileExtension >=> fileTypeByExt) (concat allFiles)
  pure $ L.nub . catMaybes $ allFileTypes

makeTemplatesDir :: MonadIO m => FilePath -> m ()
makeTemplatesDir path = createDirectory $ path </> "headroom-templates"
