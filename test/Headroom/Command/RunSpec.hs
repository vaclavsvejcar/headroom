{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Headroom.Command.RunSpec
  ( spec
  )
where

import           Headroom.Command.Run
import           Headroom.Configuration.Types        ( CtHeaderFnConfigs
                                                     , HeaderFnConfig(..)
                                                     , HeaderFnConfigs(..)
                                                     , HeaderSyntax(..)
                                                     , LicenseType(..)
                                                     , UpdateCopyrightConfig(..)
                                                     )
import           Headroom.Data.EnumExtra             ( EnumExtra(..) )
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Lens                  ( suffixLenses
                                                     , suffixLensesFor
                                                     )
import           Headroom.Data.TextExtra             ( fromLines )
import           Headroom.FileSystem                 ( FileSystem(..) )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Meta                       ( TemplateType )
import           Headroom.TemplateSupport            ( TemplateSupport(..) )
import           Headroom.Types                      ( CurrentYear(..) )
import           Headroom.Variables                  ( mkVariables )
import           RIO                          hiding ( assert )
import qualified RIO.Map                            as M
import qualified RIO.NonEmpty                       as NE
import qualified RIO.Text                           as T
import           Test.Hspec
import           Test.Hspec.QuickCheck               ( prop )
import           Test.QuickCheck              hiding ( sample )
import           Test.QuickCheck.Monadic


data TestEnv = TestEnv
  { envLogFunc         :: LogFunc
  , envCurrentYear     :: CurrentYear
  , envFileSystem      :: FileSystem (RIO TestEnv)
  , envHeaderFnConfigs :: CtHeaderFnConfigs
  }

suffixLenses ''TestEnv
suffixLensesFor ["fsFindFilesByExts", "fsLoadFile"] ''FileSystem

instance HasLogFunc TestEnv where
  logFuncL = envLogFuncL

instance Has CtHeaderFnConfigs TestEnv where
  hasLens = envHeaderFnConfigsL

instance Has CurrentYear TestEnv where
  hasLens = envCurrentYearL

instance Has (FileSystem (RIO TestEnv)) TestEnv where
  hasLens = envFileSystemL


spec :: Spec
spec = do
  describe "loadBuiltInTemplates" $ do
    it "should load correct number of built-in templates" $ do
      M.size <$> runRIO env (loadBuiltInTemplates BSD3) `shouldReturn` 11


  describe "loadTemplateFiles" $ do
    it "should load templates from given paths" $ do
      let env' =
            env
              & (envFileSystemL . fsFindFilesByExtsL .~ fsFindFilesByExts')
              & (envFileSystemL . fsLoadFileL .~ fsLoadFile')
          fsFindFilesByExts' "test-dir" _ = pure ["haskell.mustache"]
          fsFindFilesByExts' _          _ = throwString "INVALID CONDITION"
          fsLoadFile' "haskell.mustache" = pure "template content"
          fsLoadFile' _                  = throwString "INVALID CONDITION"
      templates <- runRIO env' $ loadTemplateFiles ["test-dir"]
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


  describe "postProcessHeader'" $ do
    it "should perform expected post-processing on license header" $ do
      let sample = fromLines
            [ "-- Copyright (c) 2018-2019 1st Author"
            , "Copyright (c) 2017 2nd Author"
            ]
          expected = fromLines
            [ "-- Copyright (c) 2018-2019 1st Author"
            , "-- Copyright (c) 2017-2020 2nd Author"
            ]
          vars   = mkVariables [("sndAuthor", "2nd Author")]
          syntax = LineComment "--"
      runRIO env (postProcessHeader' syntax vars sample) `shouldReturn` expected


  describe "sanitizeHeader" $ do
    it "does nothing when block comment syntax used" $ do
      let sample = fromLines ["{-", "foo", "bar", "-}"]
          syntax = BlockComment "{-" "-}"
      sanitizeHeader syntax sample `shouldBe` sample

    it "adds missing single-line comment syntax" $ do
      let sample   = fromLines ["-- first", "second", "-- third"]
          syntax   = LineComment "--"
          expected = fromLines ["-- first", "-- second", "-- third"]
      sanitizeHeader syntax sample `shouldBe` expected


env :: TestEnv
env = TestEnv { .. }
 where
  envLogFunc     = mkLogFunc (\_ _ _ _ -> pure ())
  envCurrentYear = CurrentYear 2020
  envFileSystem  = FileSystem { fsCreateDirectory     = undefined
                              , fsDoesFileExist       = undefined
                              , fsFindFiles           = undefined
                              , fsFindFilesByExts     = undefined
                              , fsFindFilesByTypes    = undefined
                              , fsGetCurrentDirectory = undefined
                              , fsListFiles           = undefined
                              , fsLoadFile            = undefined
                              }
  envHeaderFnConfigs = HeaderFnConfigs
    { hfcsUpdateCopyright = HeaderFnConfig
                              { hfcEnabled = True
                              , hfcConfig  = UpdateCopyrightConfig
                                               { uccSelectedAuthors =
                                                 Just $ "{{ sndAuthor }}" :| []
                                               }
                              }
    }
