{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Data.RegexSpec (
    spec
) where

import Headroom.Data.Regex
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "match" $ do
        it "matches regular expression against given sample" $ do
            let regex = [re|foo|bar|]
            match regex "xxx" `shouldSatisfy` isNothing
            match regex "foz" `shouldSatisfy` isNothing
            match regex "foosdas" `shouldSatisfy` isJust
            match regex "barfoo" `shouldSatisfy` isJust

    describe "isMatch" $ do
        it "checks if regular expression matches against given sample" $ do
            let regex = [re|foo|bar|]
            isMatch regex "foz" `shouldBe` False
            isMatch regex "xxx" `shouldBe` False
            isMatch regex "foosdas" `shouldBe` True
            isMatch regex "barfoo" `shouldBe` True
