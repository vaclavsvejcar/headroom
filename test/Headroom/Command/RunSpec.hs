{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Headroom.Command.RunSpec
  ( spec
  )
where

import           Headroom.Command.Run
import           Headroom.Configuration.Types        ( CtPostProcessConfigs
                                                     , HeaderSyntax(..)
                                                     , PostProcessConfig(..)
                                                     , PostProcessConfigs(..)
                                                     , UpdateCopyrightConfig(..)
                                                     )
import           Headroom.Data.EnumExtra             ( EnumExtra(..) )
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Lens                  ( suffixLenses
                                                     , suffixLensesFor
                                                     )
import           Headroom.Data.Regex                 ( re )
import           Headroom.Data.Text                  ( fromLines )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.IO.FileSystem              ( FileSystem(..) )
import           Headroom.IO.Network                 ( Network(..) )
import           Headroom.Meta                       ( TemplateType )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Template.Mustache          ( Mustache )
import           Headroom.Template.TemplateRef       ( TemplateRef(..) )
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
import           Text.URI.QQ                         ( uri )


data TestEnv = TestEnv
  { envLogFunc            :: LogFunc
  , envCurrentYear        :: CurrentYear
  , envFileSystem         :: FileSystem (RIO TestEnv)
  , envNetwork            :: Network (RIO TestEnv)
  , envPostProcessConfigs :: CtPostProcessConfigs
  }

suffixLenses ''TestEnv
suffixLensesFor ["fsFindFilesByExts", "fsLoadFile"] ''FileSystem
suffixLensesFor ["nDownloadContent"] ''Network

instance HasLogFunc TestEnv where
  logFuncL = envLogFuncL

instance Has CtPostProcessConfigs TestEnv where
  hasLens = envPostProcessConfigsL

instance Has CurrentYear TestEnv where
  hasLens = envCurrentYearL

instance Has (FileSystem (RIO TestEnv)) TestEnv where
  hasLens = envFileSystemL

instance Has (Network (RIO TestEnv)) TestEnv where
  hasLens = envNetworkL


spec :: Spec
spec = do

  describe "loadTemplateRefs" $ do
    it "should load templates from given references" $ do
      let env' =
            env
              & (envFileSystemL . fsFindFilesByExtsL .~ fsFindFilesByExts')
              & (envFileSystemL . fsLoadFileL .~ fsLoadFile')
              & (envNetworkL . nDownloadContentL .~ nDownloadContent')
          fsFindFilesByExts' = \path _ -> case path of
            "test-dir" -> pure ["haskell.mustache", "rust.mustache"]
            _          -> throwString "INVALID"
          fsLoadFile' = \case
            "haskell.mustache" -> pure "haskell local"
            "rust.mustache"    -> pure "rust\nlocal\n"
            _                  -> throwString "INVALID"
          nDownloadContent' = \case
            [uri|http://test.com/haskell.mustache|] -> pure "haskell URI"
            _ -> throwString "INVALID"
          refs =
            [ UriTemplateRef [uri|http://test.com/haskell.mustache|]
            , LocalTemplateRef "test-dir"
            ]
      templates <- runRIO env' $ loadTemplateRefs @Mustache refs
      M.size templates `shouldBe` 2
      M.member Haskell templates `shouldBe` True
      M.member Rust templates `shouldBe` True
      rawTemplate <$> M.lookup Haskell templates `shouldBe` Just "haskell local"
      rawTemplate <$> M.lookup Rust templates `shouldBe` Just "rust\nlocal"


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
          syntax = LineComment [re|^--|] (Just "--")
      runRIO env (postProcessHeader' @Mustache syntax vars sample)
        `shouldReturn` expected


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
                              , fsGetCacheDirectory   = undefined
                              , fsGetCurrentDirectory = undefined
                              , fsListFiles           = undefined
                              , fsLoadFile            = undefined
                              }
  envNetwork            = Network { nDownloadContent = undefined }
  envPostProcessConfigs = PostProcessConfigs
    { ppcsUpdateCopyright = PostProcessConfig
                              { ppcEnabled = True
                              , ppcConfig  = UpdateCopyrightConfig
                                               { uccSelectedAuthors =
                                                 Just $ "{{ sndAuthor }}" :| []
                                               }
                              }
    }
