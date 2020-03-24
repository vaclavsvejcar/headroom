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
{-# LANGUAGE TypeApplications  #-}
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
import           Headroom.Global                ( TemplateType )
import           Headroom.License               ( License(..) )
import           Headroom.Template              ( templateExtensions )
import           Headroom.Types                 ( HeadroomError(..)
                                                , InitCommandError(..)
                                                )
import           Headroom.UI.Progress           ( Progress(..)
                                                , zipWithProgress
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
import qualified RIO.NonEmpty                  as NE
import qualified RIO.Text                      as T


env' :: InitOptions -> LogFunc -> IO Env
env' opts logFunc = do
  currentDir <- getCurrentDirectory
  let paths = Paths { pCurrentDir   = currentDir
                    , pConfigFile   = ".headroom.yaml"
                    , pTemplatesDir = "headroom-templates"
                    }
  pure $ Env { envLogFunc = logFunc, envInitOptions = opts, envPaths = paths }

-- | Handler for /Init/ command.
commandInit :: InitOptions -- ^ /Init/ command options
            -> IO ()       -- ^ execution result
commandInit opts = bootstrap (env' opts) False $ doesAppConfigExist >>= \case
  False -> do
    fileTypes <- findSupportedFileTypes
    makeTemplatesDir
    createTemplates fileTypes
    createConfigFile
  True -> throwM $ InitCommandError AppConfigAlreadyExists

-- | Recursively scans provided source paths for known file types for which
-- templates can be generated.
findSupportedFileTypes :: (HasInitOptions env, HasLogFunc env)
                       => RIO env [FileType]
findSupportedFileTypes = do
  opts      <- view initOptionsL
  fileTypes <- do
    allFiles <- mapM (\path -> findFiles path (const True)) (ioSourcePaths opts)
    let allFileTypes = fmap (fileExtension >=> fileTypeByExt) (concat allFiles)
    pure $ L.nub . catMaybes $ allFileTypes
  case fileTypes of
    [] -> throwM $ InitCommandError NoSourcePaths
    _  -> do
      logInfo $ "Found supported file types: " <> displayShow fileTypes
      pure fileTypes

createTemplates :: (HasInitOptions env, HasLogFunc env, HasPaths env)
                => [FileType]
                -> RIO env ()
createTemplates fileTypes = do
  opts  <- view initOptionsL
  paths <- view pathsL
  let templatesDir = pCurrentDir paths </> pTemplatesDir paths
  mapM_ (\(p, l) -> createTemplate templatesDir l p)
        (zipWithProgress $ fmap (License (ioLicenseType opts)) fileTypes)

createTemplate :: (HasLogFunc env)
               => FilePath
               -> License
               -> Progress
               -> RIO env ()
createTemplate templatesDir license@(License _ fileType) progress = do
  let extension = NE.head $ templateExtensions @TemplateType
      file = (fmap C.toLower . show $ fileType) <> "." <> T.unpack extension
      filePath  = templatesDir </> file
      template  = licenseTemplate license
  logInfo $ mconcat
    [display progress, " Creating template file in ", fromString filePath]
  writeFileUtf8 filePath template

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

-- | Checks whether application config file already exists.
doesAppConfigExist :: (HasLogFunc env, HasPaths env) => RIO env Bool
doesAppConfigExist = do
  paths <- view pathsL
  logInfo "Verifying that there's no existing Headroom configuration..."
  doesFileExist $ pCurrentDir paths </> pConfigFile paths

-- | Creates directory for template files.
makeTemplatesDir :: (HasLogFunc env, HasPaths env) => RIO env ()
makeTemplatesDir = do
  paths <- view pathsL
  let templatesDir = pCurrentDir paths </> pTemplatesDir paths
  logInfo $ "Creating directory for templates in " <> fromString templatesDir
  createDirectory templatesDir
