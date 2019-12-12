{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.CSSSpec
  ( spec
  )
where

import           Headroom.Header.Impl.CSS
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  describe "headerSizeCSS" $ do
    it "detects size of header comment in CSS" $ do
      sample1 <- readFileUtf8 "test-data/code-samples/css/sample1.css"
      sample2 <- readFileUtf8 "test-data/code-samples/css/sample2.css"
      sample3 <- readFileUtf8 "test-data/code-samples/css/sample3.css"
      headerSizeCSS sample1 `shouldBe` 16
      headerSizeCSS sample2 `shouldBe` 2
      headerSizeCSS sample3 `shouldBe` 0

    it "handles empty files" $ do
      headerSizeCSS "" `shouldBe` 0
