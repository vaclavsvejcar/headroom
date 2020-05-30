{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Headroom.Command.RunSpec
  ( spec
  )
where

import           Headroom.Command.Run
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Meta                  ( TemplateType )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( FileType(..)
                                                , HeaderSyntax(..)
                                                , LicenseType(..)
                                                )
import           RIO                     hiding ( assert )
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Map                       as M
import qualified RIO.NonEmpty                  as NE
import qualified RIO.Text                      as T
import           Test.Hspec
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck         hiding ( sample )
import           Test.QuickCheck.Monadic


spec :: Spec
spec = do
  describe "loadBuiltInTemplates" $ do
    it "should load correct number of built-in templates" $ do
      templates <- runRIO env $ loadBuiltInTemplates BSD3
      M.size templates `shouldBe` 10


  describe "loadTemplateFiles" $ do
    it "should load templates from given paths" $ do
      templates <- runRIO env $ loadTemplateFiles ["test-data" </> "templates"]
      M.size templates `shouldBe` 1
      M.member Haskell templates `shouldBe` True


  describe "typeOfTemplate" $ do
    let fileTypes = fmap (T.toLower . enumToText) (allValues @FileType)
        templateExt         = NE.head $ templateExtensions @TemplateType
        pathGen             = elements $ fmap (<> "." <> templateExt) fileTypes
        prop_typeOfTemplate = monadicIO $ do
          path   <- T.unpack <$> pick pathGen
          result <- run (runRIO env $ typeOfTemplate path)
          assert $ isJust result

    prop "should detect type of template from template path" prop_typeOfTemplate


  describe "sanitizeHeader" $ do
    it "does nothing when block comment syntax used" $ do
      let sample = T.unlines ["{-", "foo", "bar", "-}"]
          syntax = BlockComment "{-" "-}"
      sanitizeHeader syntax sample `shouldBe` sample

    it "adds missing single-line comment syntax" $ do
      let sample   = T.unlines ["-- first", "second", "-- third"]
          syntax   = LineComment "--"
          expected = T.unlines ["-- first", "-- second", "-- third"]
      sanitizeHeader syntax sample `shouldBe` expected


env :: TestEnv
env = TestEnv { envLogFunc = logFunc }
  where logFunc = mkLogFunc (\_ _ _ _ -> pure ())

newtype TestEnv = TestEnv
  { envLogFunc :: LogFunc
  }

instance HasLogFunc TestEnv where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
