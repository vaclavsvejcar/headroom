{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.FileSupport.Haskell.HaddockSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( HeaderSyntax(..)
                                                     , LicenseType(..)
                                                     )
import           Headroom.Data.Regex                 ( re )
import           Headroom.Data.Text                  ( fromLines )
import           Headroom.Embedded                   ( licenseTemplate )
import           Headroom.FileSupport                ( analyzeSourceCode
                                                     , fileSupport
                                                     )
import           Headroom.FileSupport.Haskell.Haddock
import           Headroom.FileSupport.TemplateData   ( HaddockOffsets(..)
                                                     , TemplateData(..)
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.IO.FileSystem              ( loadFile )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Template.Mustache          ( Mustache(..) )
import           Headroom.Template.TemplateRef       ( TemplateRef(..) )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let codeSamples = "test-data" </> "code-samples"

  describe "extractOffsets" $ do
    it "extract offsets for selected fields of module header" $ do
      template <-
        parseTemplate @Mustache (BuiltInRef BSD3 Haskell)
          $ licenseTemplate BSD3 Haskell
      let syntax   = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
          expected = HaddockOffsets { hoCopyright = Just 14 }
      extractOffsets template syntax `shouldBe` expected


  describe "extractModuleHeader" $ do
    it "extracts fields from Haddock module header" $ do
      raw <- loadFile $ codeSamples </> "haskell" </> "header.hs"
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
        sample = analyzeSourceCode (fileSupport Haskell) raw
        syntax = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
      extractModuleHeader sample NoTemplateData syntax `shouldBe` expected


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

