{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.RunSpec
  ( spec
  )
where

import           Headroom.Command.Run
import           Headroom.Types                 ( FileType(..)
                                                , LicenseType(..)
                                                )
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Map                       as M
import           Test.Hspec


spec :: Spec
spec = do
  describe "loadBuiltInTemplates" $ do
    it "should load correct number of built-in templates" $ do
      templates <- runRIO env $ loadBuiltInTemplates BSD3
      M.size templates `shouldBe` 10


  describe "loadTemplateFiles" $ do
    it "should load templates from given paths" $ do
      templates <- runRIO env $ loadTemplateFiles ["test-data" </> "templates"]
      M.size templates `shouldBe` 1
      M.member Haskell templates `shouldBe` True


env :: TestEnv
env = TestEnv { envLogFunc = logFunc }
  where logFunc = mkLogFunc (\_ _ _ _ -> pure ())

newtype TestEnv = TestEnv
  { envLogFunc :: LogFunc
  }

instance HasLogFunc TestEnv where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
