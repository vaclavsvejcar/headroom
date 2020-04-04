{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.InitSpec
  ( spec
  )
where

import           Headroom.Command.Init
import           Headroom.Types                 ( CommandInitOptions(..)
                                                , FileType(HTML)
                                                , LicenseType(..)
                                                )
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.List                      as L
import           Test.Hspec


spec :: Spec
spec = do
  describe "doesAppConfigExist" $ do
    it "checks that '.headroom.yaml' exists in selected directory" $ do
      result <- runRIO env doesAppConfigExist
      result `shouldBe` True

  describe "findSupportedFileTypes" $ do
    it "recursively finds all known file types present in given path" $ do
      result <- runRIO env findSupportedFileTypes
      let expected = [HTML]
      L.sort result `shouldBe` L.sort expected

env :: Env
env = Env { envLogFunc = logFunc, envInitOptions = opts, envPaths = paths }
 where
  logFunc = mkLogFunc (\_ _ _ _ -> pure ())
  opts    = CommandInitOptions
    { cioSourcePaths = ["test-data" </> "test-traverse"]
    , cioLicenseType = BSD3
    }
  paths = Paths { pCurrentDir   = "."
                , pConfigFile   = "test-data" </> "configs" </> "full.yaml"
                , pTemplatesDir = "headroom-templates"
                }
