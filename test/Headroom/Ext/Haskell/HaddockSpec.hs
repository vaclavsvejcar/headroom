{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Ext.Haskell.HaddockSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( LicenseType(..) )
import           Headroom.Data.TextExtra             ( fromLines )
import           Headroom.Embedded                   ( licenseTemplate )
import           Headroom.Ext.Haskell.Haddock
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.TemplateSupport            ( TemplateSupport(..) )
import           Headroom.TemplateSupport.Mustache   ( Mustache(..) )
import           Headroom.Types                      ( HaddockFieldOffsets(..) )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "extractFieldOffsets" $ do
    it "extract offsets for selected fields of module header" $ do
      template <- parseTemplate @Mustache Nothing $ licenseTemplate BSD3 Haskell
      let expected = HaddockFieldOffsets { hfoCopyright = Just 14 }
      extractFieldOffsets template `shouldBe` expected


  describe "extractModuleHeader" $ do
    it "extracts fields from Haddock module header" $ do
      let
        expected = HaddockModuleHeader
          { hmhCopyright   = Just
            "(c) Some Guy, 2013\n                  Someone Else, 2014"
          , hmhLicense     = Just "GPL-3"
          , hmhMaintainer  = Just "sample@email.com"
          , hmhPortability = Just "POSIX"
          , hmhStability   = Just "experimental"
          , hmhShortDesc   = Just "Short description"
          , hmhLongDesc    =
            Just
              "Here is a longer description of this module, containing some\n\
              \commentary with @some markup@."
          }
      sample <- loadFile $ codeSamples </> "haskell" </> "header.hs"
      extractModuleHeader sample Nothing `shouldBe` expected


  describe "stripCommentSyntax" $ do
    it "strips single-line or block comment syntax from input" $ do
      let sample1 = fromLines ["{-|", "Hello1", "foo", "-}"]
          sample2 = fromLines ["{- |", "Hello2", "foo", "-}"]
          sample3 = fromLines ["-- | Hello3", "-- foo"]
      stripCommentSyntax sample1 `shouldBe` fromLines ["", "Hello1", "foo", ""]
      stripCommentSyntax sample2 `shouldBe` fromLines ["", "Hello2", "foo", ""]
      stripCommentSyntax sample3 `shouldBe` fromLines [" Hello3", " foo"]


  describe "indentField" $ do
    it "does nothing with single line text" $ do
      let sample = fromLines ["hello"]
          offset = Just 2
      indentField offset sample `shouldBe` sample

    it "indents all but first line using given offset" $ do
      let sample   = fromLines ["first", "second", "third"]
          expected = fromLines ["first", "  second", "  third"]
          offset   = Just 2
      indentField offset sample `shouldBe` expected

    it "intents correctly previously indented text" $ do
      let sample   = fromLines ["first", "second", "        third"]
          expected = fromLines ["first", "  second", "  third"]
          offset   = Just 2
      indentField offset sample `shouldBe` expected


