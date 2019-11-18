{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.HeaderSpec
  ( spec
  )
where

import           Headroom.Header
import           Headroom.Types                 ( FileType(..) )
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  describe "headerSize" $ do
    it "returns correct size of header for Haskell source file" $ do
      source <- readFileUtf8 "test-data/code-samples/Test.hs"
      headerSize Haskell source `shouldBe` 15

  describe "stripHeader" $ do
    it "strips header for Haskell source file" $ do
      source   <- readFileUtf8 "test-data/code-samples/Test.hs"
      expected <- readFileUtf8 "test-data/code-samples-striped/Test.hs"
      stripHeader Haskell source `shouldBe` expected
