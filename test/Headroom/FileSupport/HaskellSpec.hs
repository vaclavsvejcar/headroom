{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileSupport.HaskellSpec
  ( spec
  )
where

import           Headroom.FileSupport.Haskell
import           Headroom.FileSystem            ( loadFile )
import           Headroom.Types                 ( HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashMap                   as HM
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "extractModuleName" $ do
    it "extracts module name from Haskell file content" $ do
      sample1 <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      sample2 <- loadFile $ codeSamples </> "html" </> "sample2.html"
      extractModuleName sample1 `shouldBe` Just "Test"
      extractModuleName sample2 `shouldBe` Nothing


  describe "extractVariablesHaskell" $ do
    it "extracts variables from Haskell source code with Haddock header" $ do
      let config    = HeaderConfig ["hs"] 0 0 [] [] (BlockComment "{-|" "-}")
          headerPos = Just (1, 13)
          expected  = HM.fromList
            [ ("_haskell_module_name"     , "Test")
            , ("_haskell_module_longdesc" , "long\ndescription")
            , ("_haskell_module_shortdesc", "Short description")
            ]
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      extractVariablesHaskell config headerPos sample `shouldBe` expected

    it "extracts variables from Haskell source code without Haddock header" $ do
      let config    = HeaderConfig ["hs"] 0 0 [] [] (BlockComment "{-|" "-}")
          headerPos = Nothing
          expected  = HM.fromList
            [ ("_haskell_module_name"     , "Test")
            , ("_haskell_module_longdesc" , "!!! MODULE LONG DESCRIPTION !!!")
            , ("_haskell_module_shortdesc", "!!! MODULE SHORT DESCRIPTION !!!")
            ]
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      extractVariablesHaskell config headerPos sample `shouldBe` expected
