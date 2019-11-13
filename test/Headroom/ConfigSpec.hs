module Headroom.ConfigSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Config
import           Headroom.Types

spec :: Spec
spec = do
  describe "parseAppConfig" $ do
    rawAppConfig <- runIO $ loadAppConfig "test-data/test-config.json"
    it "parses configuration from JSON file" $ do
      let actual   = parseAppConfig rawAppConfig
          expected = AppConfig "Hello" "World"
      actual `shouldBe` Just expected
