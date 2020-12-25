{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.Ext.HaskellSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( HeaderConfig(..)
                                                     , HeaderSyntax(..)
                                                     )
import           Headroom.Data.Regex                 ( re )
import           Headroom.Ext.Haskell
import           Headroom.Ext.Types                  ( ExtData(..) )
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.Types               ( TemplateInfo(..) )
import           Headroom.Template                   ( emptyTemplate )
import           Headroom.Template.Mustache          ( Mustache )
import           Headroom.Variables                  ( mkVariables )
import           RIO
import           RIO.FilePath                        ( (</>) )
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


  describe "extractVariables" $ do
    it "extracts variables from Haskell source code with Haddock header" $ do
      template <- emptyTemplate @_ @Mustache
      let comment   = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
          config    = HeaderConfig ["hs"] 0 0 0 0 [] [] comment
          ti        = TemplateInfo config NoExtData Haskell template
          headerPos = Just (1, 13)
          expected  = mkVariables
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
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      extractVariables ti headerPos sample `shouldBe` expected

    it "extracts variables from Haskell source code without Haddock header" $ do
      template <- emptyTemplate @_ @Mustache
      let comment   = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
          config    = HeaderConfig ["hs"] 0 0 0 0 [] [] comment
          ti        = TemplateInfo config NoExtData Haskell template
          headerPos = Nothing
          expected  = mkVariables [("_haskell_module_name", "Test")]
      sample <- loadFile $ codeSamples </> "haskell" </> "full.hs"
      extractVariables ti headerPos sample `shouldBe` expected

