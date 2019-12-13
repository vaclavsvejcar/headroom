{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileTypeSpec
  ( spec
  )
where

import           Headroom.FileType
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "fileTypeByExt" $ do
    it "parses FileType from file extension" $ do
      fileTypeByExt "hs" `shouldBe` Just Haskell

  describe "fileTypeByName" $ do
    it "reads FileType from string representation" $ do
      let actual   = fileTypeByName "haskell"
          expected = Just Haskell
      actual `shouldBe` expected
