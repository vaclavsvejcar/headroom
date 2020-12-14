{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ConfigurationSpec
  ( spec
  )
where

import           Headroom.Configuration
import           Headroom.Embedded                   ( defaultConfig )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "parseConfiguration" $ do
    it "should parse default bundled configuration" $ do
      parseConfiguration defaultConfig `shouldSatisfy` isJust
