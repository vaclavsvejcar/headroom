module Headroom.ConfigSpec
  ( spec
  )
where

import qualified Data.Map.Strict               as Map
import           Test.Hspec
import           Headroom.Config
import           Headroom.Types

spec :: Spec
spec = do
  describe "parseAppConfig" $ do
    rawAppConfig <- runIO $ loadAppConfig "test-data/test-config.yaml"
    it "parses configuration from JSON file" $ do
      let options  = Map.fromList [("option1", "value1")]
          actual   = parseAppConfig rawAppConfig
          expected = AppConfig "Hello" "World" options
      actual `shouldBe` Right expected
