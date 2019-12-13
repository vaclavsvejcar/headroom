{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.JSSpec
  ( spec
  )
where

import           Headroom.Header.Impl.JS
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  describe "headerSizeJS" $ do
    it "detects size of header comment in JS" $ do
      sample1 <- readFileUtf8 "test-data/code-samples/css/sample1.css"
      sample2 <- readFileUtf8 "test-data/code-samples/css/sample2.css"
      sample3 <- readFileUtf8 "test-data/code-samples/css/sample3.css"
      headerSizeJS sample1 `shouldBe` 16
      headerSizeJS sample2 `shouldBe` 2
      headerSizeJS sample3 `shouldBe` 0

    it "handles empty files" $ do
      headerSizeJS "" `shouldBe` 0
