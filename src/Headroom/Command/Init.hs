{-|
Module      : Headroom.Command.Init
Description : Logic for Init command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Logic for the @init@ command, used to generate initial configuration boilerplate
for Headroom.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Command.Init
  ( commandInit
  , doesAppConfigExist
  , findSupportedFileTypes
  )
where

import           Headroom.AppConfig             ( AppConfig(..)
                                                , prettyPrintAppConfig
                                                )
import           Headroom.Command.Init.Env
import           Headroom.Command.Shared        ( bootstrap )
import           Headroom.Embedded              ( licenseTemplate )
import           Headroom.FileSystem            ( fileExtension
                                                , findFiles
                                                )
import           Headroom.FileType              ( FileType
                                                , fileTypeByExt
                                                )
import           Headroom.License               ( License(..) )
import           Headroom.Types                 ( HeadroomError(..)
                                                , InitCommandError(..)
                                                )
import           RIO
import qualified RIO.Char                      as C
import           RIO.Directory                  ( createDirectory
                                                , doesFileExist
                                                , getCurrentDirectory
                                                )
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L


env' :: InitOptions -> LogFunc -> IO Env
env' opts logFunc = do
  currentDir <- getCurrentDirectory
  let paths = Paths { pCurrentDir   = currentDir
                    , pConfigFile   = ".headroom.yaml"
                    , pTemplatesDir = "headroom-templates"
                    }
  pure $ Env { envLogFunc = logFunc, envInitOptions = opts, envPaths = paths }

commandInit :: InitOptions -> IO ()
commandInit opts = bootstrap (env' opts) False $ doesAppConfigExist >>= \case
  False -> do
    fileTypes <- findSupportedFileTypes
    makeTemplatesDir
    createTemplates fileTypes
    createConfigFile
  True -> throwM $ InitCommandError AppConfigAlreadyExists

findSupportedFileTypes :: (HasInitOptions env, HasLogFunc env)
                       => RIO env [FileType]
findSupportedFileTypes = do
  opts <- view initOptionsL
  logInfo "Searching supported file types to generate templates for..."
  fileTypes <- do
    allFiles <- mapM (\path -> findFiles path (const True)) (ioSourcePaths opts)
    let allFileTypes = fmap (fileExtension >=> fileTypeByExt) (concat allFiles)
    pure $ L.nub . catMaybes $ allFileTypes
  case fileTypes of
    [] -> throwM $ InitCommandError NoSourcePaths
    _  -> do
      logInfo $ "Done, found supported file types: " <> displayShow fileTypes
      pure fileTypes

createTemplates :: (HasInitOptions env, HasLogFunc env, HasPaths env)
                => [FileType]
                -> RIO env ()
createTemplates fileTypes = do
  opts  <- view initOptionsL
  paths <- view pathsL
  let templatesDir = pCurrentDir paths </> pTemplatesDir paths
  mapM_ (createTemplate templatesDir)
        (fmap (License (ioLicenseType opts)) fileTypes)

createConfigFile :: (HasInitOptions env, HasLogFunc env, HasPaths env)
                 => RIO env ()
createConfigFile = do
  opts  <- view initOptionsL
  paths <- view pathsL
  let filePath = pCurrentDir paths </> pConfigFile paths
      content  = prettyPrintAppConfig $ appConfig opts paths
  logInfo $ "Creating YAML config file in " <> fromString filePath
  writeFileUtf8 filePath content
 where
  variables = HM.fromList
    [ ("author" , "John Smith")
    , ("email"  , "john.smith@example.com")
    , ("project", "My project")
    , ("year"   , "2020")
    ]
  appConfig opts paths = mempty { acSourcePaths   = ioSourcePaths opts
                                , acTemplatePaths = [pTemplatesDir paths]
                                , acVariables     = variables
                                }

createTemplate :: (HasLogFunc env) => FilePath -> License -> RIO env ()
createTemplate templatesDir license@(License _ fileType) = do
  let fileName = (fmap C.toLower . show $ fileType) <> ".mustache"
      filePath = templatesDir </> fileName
      template = licenseTemplate license
  logInfo $ mconcat ["Creating template file in ", fromString filePath]
  writeFileUtf8 filePath template

doesAppConfigExist :: (HasLogFunc env, HasPaths env) => RIO env Bool
doesAppConfigExist = do
  paths <- view pathsL
  logInfo "Verifying that there's no existing Headroom configuration..."
  doesFileExist $ pCurrentDir paths </> pConfigFile paths

makeTemplatesDir :: (HasLogFunc env, HasPaths env) => RIO env ()
makeTemplatesDir = do
  paths <- view pathsL
  let templatesDir = pCurrentDir paths </> pTemplatesDir paths
  logInfo $ "Creating directory for templates in " <> fromString templatesDir
  createDirectory templatesDir
