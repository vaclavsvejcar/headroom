{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.FileSupport.HaskellSpec
  ( spec
  )
where

import           Data.String.Interpolate             ( __i )
import           Headroom.Configuration              ( makeHeadersConfig
                                                     , parseConfiguration
                                                     )
import           Headroom.Configuration.Types        ( Configuration(..)
                                                     , LicenseType(..)
                                                     )
import           Headroom.Embedded                   ( defaultConfig
                                                     , licenseTemplate
                                                     )
import           Headroom.FileSupport                ( analyzeSourceCode
                                                     , fileSupport
                                                     )
import           Headroom.FileSupport.TemplateData   ( HaddockOffsets(..)
                                                     , HaskellTemplateData'(..)
                                                     , TemplateData(..)
                                                     )
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     )
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header                     ( extractHeaderTemplate )
import           Headroom.Template                   ( Template(..)
                                                     , emptyTemplate
                                                     )
import           Headroom.Template.Mustache          ( Mustache )
import           Headroom.Variables                  ( mkVariables )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples" </> "haskell"

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
    it "provides correct custom data for Haskell" $ do
      template <- parseTemplate @Mustache Nothing (licenseTemplate BSD3 Haskell)
      let o        = Just 14
          td       = HaskellTemplateData' HaddockOffsets { hoCopyright = o }
          expected = HaskellTemplateData td
      fsExtractTemplateData fileSupport' template `shouldBe` expected


  describe "fsExtractVariables" $ do
    it "extract variables from Haskell source code" $ do
      template       <- emptyTemplate @_ @Mustache
      defaultConfig' <- parseConfiguration defaultConfig
      config         <- makeHeadersConfig (cLicenseHeaders defaultConfig')
      raw            <- loadFile $ codeSamples </> "full.hs"
      let ht = extractHeaderTemplate config Haskell template
          headerPos = Just (1, 29)
          longDesc = [__i|
              long
              description

              == Code sample
              @
              \# LANGUAGE TypeApplications \#

              module Data.VCS.Test where

              import Data.VCS.Ignore ( Git, Repo(..), listRepo )

              example :: IO [FilePath]
              example = do
                repo <- scanRepo @Git "path/to/repo"
                listRepo repo
              @
            |]
          expected = mkVariables
            [ ( "_haskell_module_copyright"
              , "(c) Some Guy, 2013\n                  Someone Else, 2014"
              )
            , ("_haskell_module_license"    , "GPL-3")
            , ("_haskell_module_maintainer" , "sample@email.com")
            , ("_haskell_module_name"       , "Test")
            , ("_haskell_module_stability"  , "experimental")
            , ("_haskell_module_portability", "POSIX")
            , ("_haskell_module_longdesc"   , longDesc)
            , ("_haskell_module_shortdesc"  , "Short description")
            ]
          sample = analyzeSourceCode fileSupport' raw
      fsExtractVariables fileSupport' ht headerPos sample `shouldBe` expected


  describe "fsFileType" $ do
    it "matches correct type for Haskell" $ do
      fsFileType fileSupport' `shouldBe` Haskell

 where
  fileSupport' = fileSupport Haskell
  checkSyntaxAnalysis (l, (s, e)) =
    let SyntaxAnalysis {..} = fsSyntaxAnalysis fileSupport'
    in  saIsCommentStart l == s && saIsCommentEnd l == e
