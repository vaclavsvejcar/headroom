{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileSupport.HaskellSpec
  ( spec
  )
where

import           Headroom.FileSupport.Haskell
import           Headroom.FileSystem            ( loadFile )
import           RIO
import           RIO.FilePath                   ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "extractModuleName" $ do
    it "extract module name from Haskell file content" $ do
      sample1 <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      sample2 <- loadFile $ codeSamples </> "html" </> "sample2.html"
      extractModuleName sample1 `shouldBe` Just "Test"
      extractModuleName sample2 `shouldBe` Nothing

