{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.HeaderSpec
  ( spec
  )
where

import           Headroom.Header
import           Headroom.Types                 ( FileType(..)
                                                , Header(..)
                                                )
import           RIO
import           RIO.FilePath
import           Test.Hspec

spec :: Spec
spec = do
  let readTemplate p = readFileUtf8 $ "test-data/code-samples/haskell" </> p
  describe "addHeader" $ do
    it "adds header to source code if no header is present" $ do
      let header = Header Haskell "-- This is header"
      source   <- readTemplate "stripped.hs"
      expected <- readTemplate "replaced-simple.hs"
      addHeader header source `shouldBe` expected

    it "does nothing if some header is already present" $ do
      let header = Header Haskell "-- This is header"
      source <- readTemplate "full.hs"
      addHeader header source `shouldBe` source

  describe "containsHeader" $ do
    it "detects whether source code header is present" $ do
      withHeader    <- readTemplate "full.hs"
      withoutHeader <- readTemplate "stripped.hs"
      containsHeader Haskell withHeader `shouldBe` True
      containsHeader Haskell withoutHeader `shouldBe` False

  describe "headerSize" $ do
    it "returns correct size of header for source code" $ do
      source <- readTemplate "full.hs"
      headerSize Haskell source `shouldBe` 15

  describe "replaceHeader" $ do
    it "replaces header in source code" $ do
      let header = Header Haskell "-- This is header"
      source   <- readTemplate "full.hs"
      expected <- readTemplate "replaced-simple.hs"
      replaceHeader header source `shouldBe` expected

  describe "stripHeader" $ do
    it "strips header from source code" $ do
      source   <- readTemplate "full.hs"
      expected <- readTemplate "stripped.hs"
      stripHeader Haskell source `shouldBe` expected
