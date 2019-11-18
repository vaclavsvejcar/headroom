{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.HaskellSpec
  ( spec
  )
where

import           Headroom.Header.Haskell
import           Test.Hspec
import           RIO

spec :: Spec
spec = do
  describe "headerSizeHaskell" $ do
    it "detects size of existing module header" $ do
      source <- readFileUtf8 "test-data/code-samples/Test.hs"
      headerSizeHaskell source `shouldBe` 15

    it "handles empty files" $ do
      headerSizeHaskell "" `shouldBe` 0
