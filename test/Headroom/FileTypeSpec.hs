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
  describe "fileTypeByExt" $ do
    it "parses FileType from file extension" $ do
      fileTypeByExt "hs" `shouldBe` Just Haskell
