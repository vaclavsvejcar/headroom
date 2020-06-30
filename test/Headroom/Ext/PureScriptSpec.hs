{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Ext.PureScriptSpec
  ( spec
  )
where

import           Headroom.Configuration.Types   ( HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           Headroom.Ext.PureScript
import           Headroom.FileSystem            ( loadFile )
import           Headroom.Variables             ( mkVariables )
import           RIO
import           RIO.FilePath                   ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "extractVariables" $ do
    it "extracts variables from PureScript source code" $ do
      let config    = HeaderConfig ["purs"] 0 0 [] [] (LineComment "--")
          meta      = Nothing
          headerPos = Just (1, 13)
          expected  = mkVariables [("_purescript_module_name", "Test")]
      sample <- loadFile $ codeSamples </> "purescript" </> "full.purs"
      extractVariables config meta headerPos sample `shouldBe` expected
