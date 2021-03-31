{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.FileSupport.JavaSpec
  ( spec
  )
where

import           Headroom.Configuration              ( makeHeadersConfig
                                                     , parseConfiguration
                                                     )
import           Headroom.Configuration.Types        ( Configuration(..) )
import           Headroom.Embedded                   ( defaultConfig )
import           Headroom.FileSupport                ( analyzeSourceCode
                                                     , fileSupport
                                                     )
import           Headroom.FileSupport.TemplateData   ( TemplateData(..) )
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header                     ( extractHeaderTemplate )
import           Headroom.IO.FileSystem              ( loadFile )
import           Headroom.Template                   ( emptyTemplate )
import           Headroom.Template.Mustache          ( Mustache )
import           Headroom.Variables                  ( mkVariables )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples" </> "java"


  describe "fsSyntaxAnalysis" $ do
    it "correctly detects comment starts/ends" $ do
      let samples =
            [ ("non comment line"             , (False, False))
            , ("// single line comment"       , (True, True))
            , ("not // single line comment"   , (False, False))
            , ("/* block comment start"       , (True, False))
            , ("block comment end */"         , (False, True))
            , ("/* block comment start/end */", (True, True))
            ]
      all checkSyntaxAnalysis samples `shouldBe` True


  describe "fsExtractTemplateData" $ do
    it "doesn't provide any custom data for Java" $ do
      template <- emptyTemplate @_ @Mustache
      let syntax   = undefined
          expected = NoTemplateData
      fsExtractTemplateData fileSupport' template syntax `shouldBe` expected


  describe "fsExtractVariables" $ do
    it "extract variables from Java source code" $ do
      template       <- emptyTemplate @_ @Mustache
      defaultConfig' <- parseConfiguration defaultConfig
      config         <- makeHeadersConfig (cLicenseHeaders defaultConfig')
      raw            <- loadFile $ codeSamples </> "sample1.java"
      let ht        = extractHeaderTemplate config Java template
          headerPos = Just (0, 2)
          expected  = mkVariables [("_java_package_name", "foo")]
          sample    = analyzeSourceCode fileSupport' raw
      fsExtractVariables fileSupport' ht headerPos sample `shouldBe` expected


  describe "fsFileType" $ do
    it "matches correct type for Java" $ do
      fsFileType fileSupport' `shouldBe` Java

 where
  fileSupport' = fileSupport Java
  checkSyntaxAnalysis (l, (s, e)) =
    let SyntaxAnalysis {..} = fsSyntaxAnalysis fileSupport'
    in  saIsCommentStart l == s && saIsCommentEnd l == e
