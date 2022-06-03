{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.ConfigSpec (
    spec
) where

import Headroom.Config
import Headroom.Embedded (defaultConfig)
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "parseAppConfig" $ do
        it "should parse default bundled configuration" $ do
            parseAppConfig defaultConfig `shouldSatisfy` isJust
