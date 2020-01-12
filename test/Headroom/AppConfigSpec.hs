{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.AppConfigSpec
  ( spec
  )
where

import           Headroom.AppConfig
import           Headroom.Types                 ( AppConfigError(..)
                                                , HeadroomError(..)
                                                , RunMode(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import           Test.Hspec
import           Test.Utils                     ( matchesException )


spec :: Spec
spec = do
  describe "loadAppConfig" $ do
    it "loads full configuration from YAML file" $ do
      appConfig <- loadAppConfig "test-data/configs/full.yaml"
      let options = HM.fromList
            [ ("copyright", "(c) 2019 John Smith")
            , ("email"    , "john.smith@example.com")
            ]
          sourcePaths = ["test-data/configs/path/to/src"]
          templatePaths =
            ["test-data/configs/path/to/dir1", "test-data/configs/path/to/dir2"]
          expected = AppConfig 1 Add sourcePaths templatePaths options
      appConfig `shouldBe` expected

  describe "parseVariables" $ do
    it "parses variables map from raw string list" $ do
      let raw = ["key1=value1", "key2=value with spaces"]
          expected =
            HM.fromList [("key1", "value1"), ("key2", "value with spaces")]
      parseVariables raw `shouldBe` Just expected

  let ph1        = HM.fromList [("key1", "value1")]
      ph2        = HM.fromList [("key2", "value2")]
      appConfig1 = AppConfig 1 Replace ["source1"] [] ph1
      appConfig2 = AppConfig 2 Add ["source2"] ["template1"] ph2
      expected'  = AppConfig
        1
        Replace
        ["source1", "source2"]
        ["template1"]
        (HM.fromList [("key1", "value1"), ("key2", "value2")])

  describe "validateAppConfig" $ do
    it "validates configuration version" $ do
      let check (Just (InvalidAppConfig [InvalidVersion 2 1])) = True
          check _ = False
      validateAppConfig appConfig2 `shouldSatisfy` matchesException check

  describe "<>" $ do
    it "joins two AppConfig records" $ do
      (appConfig1 <> appConfig2) `shouldBe` expected'

  describe "mconcat" $ do
    it "folds a list of AppConfig records" $ do
      mconcat [appConfig1, appConfig2] `shouldBe` expected'

