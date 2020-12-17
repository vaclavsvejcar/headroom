{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Ext.JavaSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( HeaderConfig(..)
                                                     , HeaderSyntax(..)
                                                     )
import           Headroom.Ext.Java
import           Headroom.Ext.Types                  ( ExtData(..) )
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.Types               ( TemplateInfo(..) )
import           Headroom.Variables                  ( mkVariables )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec



spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "extractPackageName" $ do
    it "extracts package name from Java source code" $ do
      sample <- loadFile $ codeSamples </> "java" </> "sample1.java"
      extractPackageName sample `shouldBe` Just "foo"


  describe "extractVariables" $ do
    it "extracts variables from Java source code" $ do
      let comment   = BlockComment "/*" "*/" Nothing
          config    = HeaderConfig ["java"] 0 0 0 0 [] [] comment
          ti        = TemplateInfo config NoExtData Java undefined
          headerPos = Just (0, 2)
          expected  = mkVariables [("_java_package_name", "foo")]
      sample <- loadFile $ codeSamples </> "java" </> "sample1.java"
      extractVariables ti headerPos sample `shouldBe` expected
