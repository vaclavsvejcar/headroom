{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.ScalaSpec
  ( spec
  )
where

import           Headroom.Header.Impl.Scala
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "headerSizeScala" $ do
    it "detects size of existing module header" $ do
      source <- readFileUtf8 "test-data/code-samples/scala/full.scala"
      headerSizeScala source `shouldBe` 4

    it "handles empty files" $ do
      headerSizeScala "" `shouldBe` 0
