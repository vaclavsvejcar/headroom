{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Meta.VersionSpec
  ( spec
  )
where

import           Headroom.Meta.Version
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  describe "Ord instance" $ do
    it "correctly compare two values" $ do
      compare (Version 1 2 3 4) (Version 1 2 3 4) `shouldBe` EQ
      compare (Version 0 1 0 0) (Version 0 1 0 1) `shouldBe` LT
      compare (Version 1 1 0 0) (Version 0 1 0 1) `shouldBe` GT


  describe "parseVersion" $ do
    it "parses valid Version from Text" $ do
      parseVersion "0.1.2.3" `shouldBe` Just (Version 0 1 2 3)

    it "parses Nothing for invalid input" $ do
      parseVersion "0.4" `shouldBe` Nothing


  describe "printVersion" $ do
    it "pretty prints given version" $ do
      printVersion (Version 1 2 3 4) `shouldBe` "1.2.3.4"

