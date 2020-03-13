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
import           Headroom.Types                 ( HeadroomError(..)
                                                , InitCommandError(..)
                                                )
import           RIO
import           RIO.Directory                  ( createDirectory
                                                , doesFileExist
                                                , getCurrentDirectory
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
      fileTypes <- findSupportedFileTypes'
      logInfo "Creating directory for templates 'headroom-templates'..."
      makeTemplatesDir currentDir
    True -> throwM $ InitCommandError AppConfigAlreadyExists
 where
  findSupportedFileTypes' = do
    logInfo "Searching supported file types to generate templates for..."
    fileTypes <- findSupportedFileTypes (ioSourcePaths opts)
    case fileTypes of
      [] -> throwM $ InitCommandError NoSourcePaths
      _  -> do
        logInfo $ "Done, found supported file types: " <> displayShow fileTypes
        pure fileTypes

doesAppConfigExist :: MonadIO m => FilePath -> m Bool
doesAppConfigExist dirPath = doesFileExist $ dirPath </> ".headroom.yaml"

findSupportedFileTypes :: MonadIO m => [FilePath] -> m [FileType]
findSupportedFileTypes paths = do
  allFiles <- mapM (\path -> findFiles path (const True)) paths
  let allFileTypes = fmap (fileExtension >=> fileTypeByExt) (concat allFiles)
  pure $ L.nub . catMaybes $ allFileTypes

makeTemplatesDir :: MonadIO m => FilePath -> m ()
makeTemplatesDir path = createDirectory $ path </> "headroom-templates"
