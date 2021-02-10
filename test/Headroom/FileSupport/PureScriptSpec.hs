{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.FileSupport.PureScriptSpec
  ( spec
  )
where

import           Headroom.Configuration              ( makeHeadersConfig
                                                     , parseConfiguration
                                                     )
import           Headroom.Configuration.Types        ( Configuration(..) )
import           Headroom.Embedded                   ( defaultConfig )
import           Headroom.FileSupport.PureScript
import           Headroom.FileSupport.TemplateData   ( TemplateData(..) )
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     )
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header                     ( extractHeaderTemplate )
import           Headroom.Template                   ( emptyTemplate )
import           Headroom.Template.Mustache          ( Mustache )
import           Headroom.Variables                  ( mkVariables )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples" </> "purescript"


  describe "fsSyntaxAnalysis" $ do
    it "correctly detects comment starts/ends" $ do
      let samples =
            [ ("non comment line"             , (False, False))
            , ("-- single line comment"       , (True, True))
            , ("not -- single line comment"   , (False, False))
            , ("{- block comment start"       , (True, False))
            , ("block comment end -}"         , (False, True))
            , ("{- block comment start/end -}", (True, True))
            ]
      all checkSyntaxAnalysis samples `shouldBe` True


  describe "fsExtractTemplateData" $ do
    it "doesn't provide any custom data for PureScript" $ do
      template <- emptyTemplate @_ @Mustache
      fsExtractTemplateData fileSupport template `shouldBe` NoTemplateData


  describe "fsExtractVariables" $ do
    it "extract variables from PureScript source code" $ do
      template       <- emptyTemplate @_ @Mustache
      defaultConfig' <- parseConfiguration defaultConfig
      config         <- makeHeadersConfig (cLicenseHeaders defaultConfig')
      sample         <- loadFile $ codeSamples </> "full.purs"
      let ht        = extractHeaderTemplate config PureScript template
          headerPos = Just (1, 13)
          expected  = mkVariables [("_purescript_module_name", "Test")]
      fsExtractVariables fileSupport ht headerPos sample `shouldBe` expected


  describe "fsFileType" $ do
    it "matches correct type for PureScript" $ do
      fsFileType fileSupport `shouldBe` PureScript

 where
  checkSyntaxAnalysis (l, (s, e)) =
    let SyntaxAnalysis {..} = fsSyntaxAnalysis fileSupport
    in  saIsCommentStart l == s && saIsCommentEnd l == e
