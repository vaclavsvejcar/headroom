{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Command.InitSpec (
    spec
) where

import Headroom.Command.Init
import Headroom.Command.Types (CommandInitOptions (..))
import Headroom.Config.Types (LicenseType (..))
import Headroom.Data.Has (Has (..))
import Headroom.Data.Lens (
    suffixLenses
    , suffixLensesFor
 )
import Headroom.FileType.Types (FileType (..))
import Headroom.IO.FileSystem (FileSystem (..))
import RIO
import RIO.FilePath ((</>))
import qualified RIO.List as L
import Test.Hspec

data TestEnv = TestEnv
    { envLogFunc :: LogFunc
    , envFileSystem :: FileSystem (RIO TestEnv)
    , envInitOptions :: CommandInitOptions
    , envPaths :: Paths
    }

suffixLenses ''TestEnv
suffixLensesFor ["fsDoesFileExist"] ''FileSystem
suffixLensesFor ["pConfigFile"] ''Paths

instance HasLogFunc TestEnv where
    logFuncL = envLogFuncL

instance Has CommandInitOptions TestEnv where
    hasLens = envInitOptionsL

instance Has (FileSystem (RIO TestEnv)) TestEnv where
    hasLens = envFileSystemL

instance Has Paths TestEnv where
    hasLens = envPathsL

spec :: Spec
spec = do
    describe "doesAppConfigExist" $ do
        it "checks that configuration file exists in selected directory" $ do
            let env' = env & envFileSystemL . fsDoesFileExistL .~ check
                check path = pure $ env' ^. envPathsL . pConfigFileL == path
            runRIO env' doesAppConfigExist `shouldReturn` True

    describe "findSupportedFileTypes" $ do
        it "recursively finds all known file types present in given path" $ do
            L.sort <$> runRIO env findSupportedFileTypes `shouldReturn` [HTML]

env :: TestEnv
env = TestEnv{..}
  where
    envLogFunc = mkLogFunc (\_ _ _ _ -> pure ())
    envInitOptions =
        CommandInitOptions
            { cioSourcePaths = ["test-data" </> "test-traverse"]
            , cioLicenseType = BSD3
            }
    envPaths =
        Paths
            { pConfigFile = "test-data" </> "configs" </> "full.yaml"
            , pTemplatesDir = "headroom-templates"
            }
    envFileSystem =
        FileSystem
            { fsCreateDirectory = undefined
            , fsDoesFileExist = undefined
            , fsFindFiles = undefined
            , fsFindFilesByExts = undefined
            , fsFindFilesByTypes = undefined
            , fsGetCurrentDirectory = undefined
            , fsGetUserDirectory = undefined
            , fsListFiles = undefined
            , fsLoadFile = undefined
            , fsWriteFile = undefined
            }
