{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.JavaSpec
  ( spec
  )
where

import           Headroom.Header.Impl.Java
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "headerSizeJava" $ do
    it "detects size of existing module header" $ do
      source <- readFileUtf8 "test-data/code-samples/java/full.java"
      headerSizeJava source `shouldBe` 4

    it "handles empty files" $ do
      headerSizeJava "" `shouldBe` 0
