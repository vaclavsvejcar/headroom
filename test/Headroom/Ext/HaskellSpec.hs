{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Ext.HaskellSpec
  ( spec
  )
where

import           Headroom.Ext.Haskell
import           Headroom.FileSystem            ( loadFile )
import           Headroom.Types                 ( CurrentYear(..)
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           Headroom.Variables             ( mkVariables )
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Text                      as T
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"
      currYear    = CurrentYear 2020

  describe "extractModuleName" $ do
    it "extracts module name from Haskell file content" $ do
      sample1 <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      sample2 <- loadFile $ codeSamples </> "html" </> "sample2.html"
      extractModuleName sample1 `shouldBe` Just "Test"
      extractModuleName sample2 `shouldBe` Nothing


  describe "extractVariables" $ do
    it "extracts variables from Haskell source code with Haddock header" $ do
      let config    = HeaderConfig ["hs"] 0 0 [] [] (BlockComment "{-|" "-}")
          meta      = Nothing
          headerPos = Just (1, 13)
          expected  = mkVariables
            [ ( "_haskell_module_copyright"
              , "(c) Some Guy, 2013\n                  Someone Else, 2014"
              )
            , ("_haskell_module_name"     , "Test")
            , ("_haskell_module_longdesc" , "long\ndescription")
            , ("_haskell_module_shortdesc", "Short description")
            ]
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      extractVariables config meta headerPos sample `shouldBe` expected

    it "extracts variables from Haskell source code without Haddock header" $ do
      let config    = HeaderConfig ["hs"] 0 0 [] [] (BlockComment "{-|" "-}")
          meta      = Nothing
          headerPos = Nothing
          expected  = mkVariables [("_haskell_module_name", "Test")]
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      extractVariables config meta headerPos sample `shouldBe` expected


  describe "updateYears" $ do
    it "does nothing on up-to-date year" $ do
      let sample = "Copyright (c) 2020"
      updateYears currYear sample `shouldBe` sample

    it "does nothing on up-to-date year range" $ do
      let sample = "Copyright (c) 2018-2020"
      updateYears currYear sample `shouldBe` sample

    it "updates outdated year" $ do
      let sample   = "Copyright (c) 2019"
          expected = "Copyright (c) 2019-2020"
      updateYears currYear sample `shouldBe` expected

    it "updates outdated year range" $ do
      let sample   = "Copyright (c) 2017-2019"
          expected = "Copyright (c) 2017-2020"
      updateYears currYear sample `shouldBe` expected

    it "updates complex multi-line text" $ do
      let sample = T.unlines
            [ "Copyright (c) 2019"
            , "Copyright (c) 2020"
            , "Copyright (c) 2019-2020"
            , "Copyright (c) 2017-2019"
            ]
          expected = T.unlines
            [ "Copyright (c) 2019-2020"
            , "Copyright (c) 2020"
            , "Copyright (c) 2019-2020"
            , "Copyright (c) 2017-2020"
            ]
      updateYears currYear sample `shouldBe` expected
