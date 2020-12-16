{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Ext.PureScriptSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( HeaderConfig(..)
                                                     , HeaderSyntax(..)
                                                     )
import           Headroom.Ext.PureScript
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

  describe "extractVariables" $ do
    it "extracts variables from PureScript source code" $ do
      let config    = HeaderConfig ["purs"] 0 0 0 0 [] [] (LineComment "--")
          ti        = TemplateInfo config NoExtData Java undefined
          headerPos = Just (1, 13)
          expected  = mkVariables [("_purescript_module_name", "Test")]
      sample <- loadFile $ codeSamples </> "purescript" </> "full.purs"
      extractVariables ti headerPos sample `shouldBe` expected
