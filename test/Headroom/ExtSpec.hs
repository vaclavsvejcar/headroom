{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ExtSpec
  ( spec
  )
where

import           Headroom.Ext
import           Headroom.Types                 ( CurrentYear(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           Headroom.Variables             ( mkVariables )
import           RIO
import           RIO.FilePath                   ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  describe "extractVariables" $ do
    it "extracts variables specific for Haskell file type" $ do
      let
        currYear   = CurrentYear 2020
        samplesDir = "test-data" </> "code-samples"
        config     = HeaderConfig ["hs"] 0 0 [] [] (LineComment "--")
        expected   = mkVariables
          [ ( "_haskell_module_copyright"
            , "(c) Some Guy, 2013\n                  Someone Else, 2014"
            )
          , ( "_haskell_module_copyright_updated"
            , "(c) Some Guy, 2013-2020\n                  Someone Else, 2014-2020"
            )
          , ("_haskell_module_name"     , "Test")
          , ("_haskell_module_longdesc" , "long\ndescription")
          , ("_haskell_module_shortdesc", "Short description")
          ]
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractVariables Haskell config (Just (1, 13)) currYear sample
        `shouldBe` expected
