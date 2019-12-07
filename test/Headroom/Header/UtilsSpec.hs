{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.UtilsSpec
  ( spec
  )
where

import           Headroom.Header.Utils
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Text                      as T
import           Test.Hspec



spec :: Spec
spec = do
  let readTemplate p = readFileUtf8 $ "test-data/code-samples/haskell" </> p

  describe "findLine" $ do
    it "should find first line matching given predicate" $ do
      text <- readTemplate "full.hs"
      findLine ("{-#" `T.isPrefixOf`) text `shouldBe` 15

    it "should return 0 for empty input" $ do
      findLine ("{-#" `T.isPrefixOf`) "" `shouldBe` 0

  describe "findLineStartingWith" $ do
    it "should find first line matching one of given prefixes" $ do
      text <- readTemplate "full.hs"
      let prefixes = ["{-#", "module"]
      findLineStartingWith prefixes text `shouldBe` 15

