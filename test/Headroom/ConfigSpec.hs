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
    it "parses configuration from JSON file" $ do
      rawAppConfig <- loadAppConfig "test-data/test-config.yaml"
      let options  = HM.fromList [("option1", "value1")]
          actual   = parseAppConfig rawAppConfig
          expected = AppConfig "Hello" "World" options
      actual `shouldBe` Right expected
