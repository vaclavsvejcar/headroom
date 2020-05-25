{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Headroom.Command.RunSpec
  ( spec
  )
where

import           Data.Time.Calendar             ( toGregorian
                                                , toGregorian
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.LocalTime            ( getCurrentTimeZone
                                                , localDay
                                                , utcToLocalTime
                                                )
import           Headroom.Command.Run
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Meta                  ( TemplateType )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( FileType(..)
                                                , LicenseType(..)
                                                )
import           Headroom.Variables             ( mkVariables )
import           RIO                     hiding ( assert )
import           RIO.FilePath                   ( (</>) )
import qualified RIO.Map                       as M
import qualified RIO.NonEmpty                  as NE
import qualified RIO.Text                      as T
import           Test.Hspec
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic



spec :: Spec
spec = do
  describe "compileVariables" $ do
    it "compiles template-like variable values" $ do
      let sample1 = mkVariables
            [("name", "John Smith"), ("greeting", "Hello, {{ name }}")]
          expected = mkVariables
            [("name", "John Smith"), ("greeting", "Hello, John Smith")]
      result <- compileVariables sample1
      result `shouldBe` expected

    it "doesn't get stuck in infinite loop on invalid recursive variable" $ do
      let sample1  = mkVariables [("greeting", "Hello, {{ greeting }}")]
          expected = mkVariables [("greeting", "Hello, Hello, {{ greeting }}")]
      result <- compileVariables sample1
      result `shouldBe` expected


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


  describe "dynamicVariables" $ do
    it "returns map of all expected dynamic variables" $ do
      actual   <- dynamicVariables
      now      <- liftIO getCurrentTime
      timezone <- liftIO getCurrentTimeZone
      let zoneNow      = utcToLocalTime timezone now
          (year, _, _) = toGregorian $ localDay zoneNow
          expected     = mkVariables [("_current_year", tshow year)]
      actual `shouldBe` expected


env :: TestEnv
env = TestEnv { envLogFunc = logFunc }
  where logFunc = mkLogFunc (\_ _ _ _ -> pure ())

newtype TestEnv = TestEnv
  { envLogFunc :: LogFunc
  }

instance HasLogFunc TestEnv where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
