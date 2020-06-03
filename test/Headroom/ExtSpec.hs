{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ExtSpec
  ( spec
  )
where

import           Headroom.Ext
import           Headroom.Types                 ( FileType(..)
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
      let samplesDir = "test-data" </> "code-samples"
          config     = HeaderConfig ["hs"] 0 0 [] [] (LineComment "--")
          meta       = Nothing
          expected   = mkVariables
            [ ( "_haskell_module_copyright"
              , "(c) Some Guy, 2013\n                  Someone Else, 2014"
              )
            , ("_haskell_module_name"     , "Test")
            , ("_haskell_module_longdesc" , "long\ndescription")
            , ("_haskell_module_shortdesc", "Short description")
            ]
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractVariables Haskell config meta (Just (1, 13)) sample
        `shouldBe` expected
