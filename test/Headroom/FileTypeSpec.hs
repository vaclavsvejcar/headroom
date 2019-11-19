{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileTypeSpec
  ( spec
  )
where

import           Headroom.FileType
import           Headroom.Types                 ( FileType(..) )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "parseFileType" $ do
    it "parses FileType from string representation" $ do
      let actual   = parseFileType "haskell"
          expected = Just Haskell
      actual `shouldBe` expected
