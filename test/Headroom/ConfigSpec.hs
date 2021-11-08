{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ConfigSpec
  ( spec
  )
where

import           Headroom.Config
import           Headroom.Embedded                   ( defaultConfig )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "parseConfiguration" $ do
    it "should parse default bundled configuration" $ do
      parseConfiguration defaultConfig `shouldSatisfy` isJust
