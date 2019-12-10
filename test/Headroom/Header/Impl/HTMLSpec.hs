{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.HTMLSpec
  ( spec
  )
where

import           Headroom.Header.Impl.HTML
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "headerSizeHTML" $ do
    it "detects size of header comment in HTML with DOCTYPE" $ do
      source <- readFileUtf8 "test-data/code-samples/html/with-doctype.html"
      headerSizeHTML source `shouldBe` 4

    it "detects size of header comment in HTML without DOCTYPE" $ do
      source <- readFileUtf8 "test-data/code-samples/html/without-doctype.html"
      headerSizeHTML source `shouldBe` 5

    it "handles empty files" $ do
      headerSizeHTML "" `shouldBe` 0
