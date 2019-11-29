{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.AppConfigSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.AppConfig
import           Headroom.Types
import           RIO
import qualified RIO.HashMap                   as HM


spec :: Spec
spec = do
  describe "parseAppConfig" $ do
    it "parses full configuration from YAML file" $ do
      appConfig <- loadAppConfig "test-data/configs/full.yaml"
      let options = HM.fromList
            [ ("copyright", "(c) 2019 John Smith")
            , ("email"    , "john.smith@example.com")
            ]
          sourcePaths   = ["path/to/src"]
          templatePaths = ["path/to/dir1", "path/to/dir2"]
          expected      = AppConfig 1 True sourcePaths templatePaths options
      appConfig `shouldBe` expected

  describe "parsePlaceholders" $ do
    it "parses placeholders map from raw string list" $ do
      let raw = ["key1=value1", "key2=value with spaces"]
          expected =
            HM.fromList [("key1", "value1"), ("key2", "value with spaces")]
      parsePlaceholders raw `shouldBe` Just expected
