{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ConfigSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Config
import           Headroom.Types
import           RIO
import qualified RIO.HashMap                   as HM


spec :: Spec
spec = do
  describe "parseAppConfig" $ do
    it "parses full configuration from YAML file" $ do
      rawAppConfig <- loadAppConfig "test-data/configs/full.yaml"
      let options = HM.fromList
            [ ("copyright", "(c) 2019 John Smith")
            , ("email"    , "john.smith@example.com")
            ]
          actual   = parseAppConfig rawAppConfig
          expected = AppConfig 1 options
      actual `shouldBe` Right expected
