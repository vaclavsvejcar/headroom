{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Command.Init
  ( Env(..)
  , Paths(..)
  , commandInit
  , doesAppConfigExist
  , findSupportedFileTypes
  )
where

import           Headroom.Command.Utils         ( bootstrap )
import           Headroom.Embedded              ( licenseTemplate )
import           Headroom.FileSystem            ( createDirectory
                                                , doesFileExist
                                                , fileExtension
                                                , findFiles
                                                , getCurrentDirectory
                                                )
import           Headroom.FileType              ( fileTypeByExt )
import           Headroom.Meta                  ( TemplateType )
import           Headroom.Serialization         ( prettyPrintYAML )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( ApplicationError(..)
                                                , CommandInitError(..)
                                                , CommandInitOptions(..)
                                                , Configuration(..)
                                                , FileType(..)
                                                , LicenseType(..)
                                                , RunMode(..)
                                                )
import           Headroom.UI                    ( Progress(..)
                                                , zipWithProgress
                                                )
import           RIO
import qualified RIO.Char                      as C
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.NonEmpty                  as NE
import qualified RIO.Text                      as T




-- | /RIO/ Environment for the /Init/ command.
data Env = Env
  { envLogFunc     :: !LogFunc
  , envInitOptions :: !CommandInitOptions
  , envPaths       :: !Paths
  }

-- | Paths to various locations of file system.
data Paths = Paths
  { pCurrentDir   :: !FilePath
  , pConfigFile   :: !FilePath
  , pTemplatesDir :: !FilePath
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

-- | Environment value with /Init/ command options.
class HasInitOptions env where
  initOptionsL :: Lens' env CommandInitOptions

-- | Environment value with 'Paths'.
class HasPaths env where
  pathsL :: Lens' env Paths

instance HasInitOptions Env where
  initOptionsL = lens envInitOptions (\x y -> x { envInitOptions = y })

instance HasPaths Env where
  pathsL = lens envPaths (\x y -> x { envPaths = y })

--------------------------------------------------------------------------------

env' :: CommandInitOptions -> LogFunc -> IO Env
env' opts logFunc = do
  currentDir <- getCurrentDirectory
  let paths = Paths { pCurrentDir   = currentDir
                    , pConfigFile   = ".headroom.yaml"
                    , pTemplatesDir = "headroom-templates"
                    }
  pure $ Env { envLogFunc = logFunc, envInitOptions = opts, envPaths = paths }

-- | Handler for /Init/ command.
commandInit :: CommandInitOptions -- ^ /Init/ command options
            -> IO ()              -- ^ execution result
commandInit opts = bootstrap (env' opts) False $ doesAppConfigExist >>= \case
  False -> do
    fileTypes <- findSupportedFileTypes
    makeTemplatesDir
    createTemplates fileTypes
    createConfigFile
  True -> do
    paths <- view pathsL
    throwM $ CommandInitError (AppConfigAlreadyExists $ pConfigFile paths)

-- | Recursively scans provided source paths for known file types for which
-- templates can be generated.
findSupportedFileTypes :: (HasInitOptions env, HasLogFunc env)
                       => RIO env [FileType]
findSupportedFileTypes = do
  opts      <- view initOptionsL
  fileTypes <- do
    allFiles <- mapM (\path -> findFiles path (const True))
                     (cioSourcePaths opts)
    let allFileTypes = fmap (fileExtension >=> fileTypeByExt) (concat allFiles)
    pure $ L.nub . catMaybes $ allFileTypes
  case fileTypes of
    [] -> throwM $ CommandInitError NoProvidedSourcePaths
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
  mapM_ (\(p, lf) -> createTemplate templatesDir lf p)
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

createConfigFile :: (HasInitOptions env, HasLogFunc env, HasPaths env)
                 => RIO env ()
createConfigFile = do
  opts  <- view initOptionsL
  paths <- view pathsL
  let filePath = pCurrentDir paths </> pConfigFile paths
      content  = prettyPrintYAML $ configuration opts paths
  logInfo $ "Creating YAML config file in " <> fromString filePath
  writeFileUtf8 filePath content
 where
  variables = HM.fromList
    [ ("author" , "John Smith")
    , ("email"  , "john.smith@example.com")
    , ("project", "My project")
    , ("year"   , "2020")
    ]
  configuration opts paths = Configuration
    { cRunMode       = Add
    , cSourcePaths   = cioSourcePaths opts
    , cTemplatePaths = [pTemplatesDir paths]
    , cVariables     = variables
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
