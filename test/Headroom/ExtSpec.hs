{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ExtSpec
  ( spec
  )
where

import           Headroom.Configuration.Types   ( HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           Headroom.Ext
import           Headroom.FileType.Types        ( FileType(..) )
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
            , ("_haskell_module_license"    , "GPL-3")
            , ("_haskell_module_maintainer" , "sample@email.com")
            , ("_haskell_module_name"       , "Test")
            , ("_haskell_module_stability"  , "experimental")
            , ("_haskell_module_portability", "POSIX")
            , ("_haskell_module_longdesc"   , "long\ndescription")
            , ("_haskell_module_shortdesc"  , "Short description")
            ]
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractVariables Haskell config meta (Just (1, 13)) sample
        `shouldBe` expected

    it "extracts variables specific for PureScript file type" $ do
      let samplesDir = "test-data" </> "code-samples"
          config     = HeaderConfig ["purs"] 0 0 [] [] (LineComment "--")
          meta       = Nothing
          expected   = mkVariables [("_purescript_module_name", "Test")]
      sample <- readFileUtf8 $ samplesDir </> "purescript" </> "full.purs"
      extractVariables PureScript config meta (Just (1, 1)) sample
        `shouldBe` expected

