{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.InitSpec
  ( spec
  )
where

import           Headroom.Command.Init
import           Headroom.FileType              ( FileType(HTML) )
import           RIO
import qualified RIO.List                      as L
import           Test.Hspec

spec :: Spec
spec = do
  describe "doesAppConfigExist" $ do
    it "checks that '.headroom.yaml' exists in selected directory" $ do
      result1 <- doesAppConfigExist "."
      result2 <- doesAppConfigExist "test-data"
      result1 `shouldBe` True
      result2 `shouldBe` False

  describe "findSupportedFileTypes" $ do
    it "recursively finds all known file types present in given path" $ do
      result <- findSupportedFileTypes ["test-data/test-traverse"]
      let expected = [HTML]
      L.sort result `shouldBe` L.sort expected
