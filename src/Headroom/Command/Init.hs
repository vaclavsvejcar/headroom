{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

{-|
Module      : Headroom.Command.Init
Description : Handler for the @init@ command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module representing the @init@ command, responsible for generating all the
required files (configuration, templates) for the given project, which are then
required by the @run@ or @gen@ commands.
-}

module Headroom.Command.Init
  ( Env(..)
  , Paths(..)
  , commandInit
  , doesAppConfigExist
  , findSupportedFileTypes
  )
where

import           Headroom.Command.Types         ( CommandInitOptions(..) )
import           Headroom.Command.Utils         ( bootstrap )
import           Headroom.Configuration         ( makeHeadersConfig
                                                , parseConfiguration
                                                )
import           Headroom.Configuration.Types   ( Configuration(..)
                                                , LicenseType(..)
                                                )
import           Headroom.Data.Has              ( Has(..) )
import           Headroom.Data.Lens             ( suffixLenses )
import           Headroom.Embedded              ( configFileStub
                                                , defaultConfig
                                                , licenseTemplate
                                                )
import           Headroom.FileSystem            ( createDirectory
                                                , doesFileExist
                                                , fileExtension
                                                , findFiles
                                                )
import           Headroom.FileType              ( fileTypeByExt )
import           Headroom.FileType.Types        ( FileType(..) )
import           Headroom.Meta                  ( TemplateType )
import           Headroom.Serialization         ( prettyPrintYAML )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( fromHeadroomError
                                                , toHeadroomError
                                                )
import           Headroom.UI                    ( Progress(..)
                                                , zipWithProgress
                                                )
import           RIO
import qualified RIO.Char                      as C
import           RIO.FilePath                   ( (</>) )
import qualified RIO.List                      as L
import qualified RIO.Map                       as M
import qualified RIO.NonEmpty                  as NE
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP


-- | /RIO/ Environment for the @init@ command.
data Env = Env
  { envLogFunc     :: !LogFunc
  , envInitOptions :: !CommandInitOptions
  , envPaths       :: !Paths
  }

-- | Paths to various locations of file system.
data Paths = Paths
  { pConfigFile   :: !FilePath
  , pTemplatesDir :: !FilePath
  }

suffixLenses ''Env

instance HasLogFunc Env where
  logFuncL = envLogFuncL

instance Has CommandInitOptions Env where
  hasLens = envInitOptionsL

instance Has Paths Env where
  hasLens = envPathsL

--------------------------------------------------------------------------------

env' :: CommandInitOptions -> LogFunc -> IO Env
env' opts logFunc = do
  let paths = Paths { pConfigFile   = ".headroom.yaml"
                    , pTemplatesDir = "headroom-templates"
                    }
  pure $ Env { envLogFunc = logFunc, envInitOptions = opts, envPaths = paths }

-- | Handler for @init@ command.
commandInit :: CommandInitOptions
            -- ^ @init@ command options
            -> IO ()
            -- ^ execution result
commandInit opts = bootstrap (env' opts) False $ doesAppConfigExist >>= \case
  False -> do
    fileTypes <- findSupportedFileTypes
    makeTemplatesDir
    createTemplates fileTypes
    createConfigFile
  True -> do
    paths <- viewL
    throwM . AppConfigAlreadyExists $ pConfigFile paths

-- | Recursively scans provided source paths for known file types for which
-- templates can be generated.
findSupportedFileTypes :: (Has CommandInitOptions env, HasLogFunc env)
                       => RIO env [FileType]
findSupportedFileTypes = do
  opts           <- viewL
  pHeadersConfig <- cLicenseHeaders <$> parseConfiguration defaultConfig
  headersConfig  <- makeHeadersConfig pHeadersConfig
  fileTypes      <- do
    allFiles <- mapM (\path -> findFiles path (const True))
                     (cioSourcePaths opts)
    let allFileTypes = fmap (fileExtension >=> fileTypeByExt headersConfig)
                            (concat allFiles)
    pure . L.nub . catMaybes $ allFileTypes
  case fileTypes of
    [] -> throwM NoProvidedSourcePaths
    _  -> do
      logInfo $ "Found supported file types: " <> displayShow fileTypes
      pure fileTypes

createTemplates :: (Has CommandInitOptions env, HasLogFunc env, Has Paths env)
                => [FileType]
                -> RIO env ()
createTemplates fileTypes = do
  opts       <- viewL
  Paths {..} <- viewL
  mapM_ (\(p, lf) -> createTemplate pTemplatesDir lf p)
        (zipWithProgress $ fmap (cioLicenseType opts, ) fileTypes)

createTemplate :: (HasLogFunc env)
               => FilePath
               -> (LicenseType, FileType)
               -> Progress
               -> RIO env ()
createTemplate templatesDir (licenseType, fileType) progress = do
  let extension = NE.head $ templateExtensions @TemplateType
      file = (fmap C.toLower . show $ fileType) <> "." <> T.unpack extension
      filePath  = templatesDir </> file
      template  = licenseTemplate licenseType fileType
  logInfo $ mconcat
    [display progress, " Creating template file in ", fromString filePath]
  writeFileUtf8 filePath template

createConfigFile :: (Has CommandInitOptions env, HasLogFunc env, Has Paths env)
                 => RIO env ()
createConfigFile = do
  opts         <- viewL
  p@Paths {..} <- viewL
  logInfo $ "Creating YAML config file in " <> fromString pConfigFile
  writeFileUtf8 pConfigFile (configuration opts p)
 where
  configuration opts paths =
    let withSourcePaths = TP.replace
          "source-paths: []"
          (toYamlList "source-paths" $ cioSourcePaths opts)
        withTemplatePaths = TP.replace
          "template-paths: []"
          (toYamlList "template-paths" [pTemplatesDir paths])
    in  withTemplatePaths . withSourcePaths $ configFileStub
  toYamlList field list =
    T.stripEnd . prettyPrintYAML $ M.fromList [(field :: Text, list)]

-- | Checks whether application config file already exists.
doesAppConfigExist :: (HasLogFunc env, Has Paths env) => RIO env Bool
doesAppConfigExist = do
  Paths {..} <- viewL
  logInfo "Verifying that there's no existing Headroom configuration..."
  doesFileExist pConfigFile

-- | Creates directory for template files.
makeTemplatesDir :: (HasLogFunc env, Has Paths env) => RIO env ()
makeTemplatesDir = do
  Paths {..} <- viewL
  logInfo $ "Creating directory for templates in " <> fromString pTemplatesDir
  createDirectory pTemplatesDir


---------------------------------  Error Types  --------------------------------

-- | Exception specific to the "Headroom.Command.Init" module
data CommandInitError
  = AppConfigAlreadyExists !FilePath
  -- ^ application configuration file already exists
  | NoProvidedSourcePaths
  -- ^ no paths to source code files provided
  | NoSupportedFileType
  -- ^ no supported file types found on source paths
  deriving (Eq, Show)

instance Exception CommandInitError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError

displayException' :: CommandInitError -> String
displayException' = T.unpack . \case
  AppConfigAlreadyExists path -> appConfigAlreadyExists path
  NoProvidedSourcePaths       -> noProvidedSourcePaths
  NoSupportedFileType         -> noSupportedFileType
 where
  appConfigAlreadyExists path =
    mconcat ["Configuration file '", T.pack path, "' already exists"]
  noProvidedSourcePaths = "No source code paths (files or directories) defined"
  noSupportedFileType   = "No supported file type found in scanned source paths"
