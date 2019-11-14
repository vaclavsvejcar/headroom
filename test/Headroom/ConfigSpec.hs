{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.ConfigSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Config
import           Headroom.Types
import           RIO
import qualified RIO.Map                       as Map


spec :: Spec
spec = do
  describe "parseAppConfig" $ do
    rawAppConfig <- runIO $ loadAppConfig "test-data/test-config.yaml"
    it "parses configuration from JSON file" $ do
      let options  = Map.fromList [("option1", "value1")]
          actual   = parseAppConfig rawAppConfig
          expected = AppConfig "Hello" "World" options
      actual `shouldBe` Right expected
