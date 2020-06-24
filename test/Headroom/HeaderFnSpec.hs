{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Headroom.HeaderFnSpec
  ( spec
  )
where

import           Headroom.Configuration.Types   ( HeaderFnConfig(..)
                                                , HeaderFnConfigs(..)
                                                , UpdateCopyrightConfig(..)
                                                )
import           Headroom.Data.Has              ( Has(..) )
import           Headroom.Data.TextExtra        ( fromLines )
import           Headroom.HeaderFn
import           Headroom.HeaderFn.Types
import           Headroom.HeaderFn.UpdateCopyright
import           Headroom.Types                 ( CurrentYear(..) )
import           Headroom.Variables             ( mkVariables )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  let currentYear = CurrentYear 2020
      mode        = UpdateSelectedAuthors . SelectedAuthors $ "2nd Author" :| []
      vars        = mkVariables [("sndAuthor", "2nd Author")]
      configs a = HeaderFnConfigs
        { hfcsUpdateCopyright = HeaderFnConfig
                                  { hfcEnabled = True
                                  , hfcConfig  = UpdateCopyrightConfig
                                                   { uccSelectedAuthors = Just
                                                                          $  a
                                                                          :| []
                                                   }
                                  }
        }
      configuredEnv a = ConfiguredEnv { ceCurrentYear         = currentYear
                                      , ceHeaderFnConfigs     = configs a
                                      , ceUpdateCopyrightMode = mode
                                      }


  describe "runHeaderFn" $ do
    it "executes the function for given environment" $ do
      let testEnv  = TestEnv "ENV"
          input    = "input"
          actual   = runHeaderFn testFn testEnv input
          expected = "input_ENV"
      actual `shouldBe` expected


  describe "postProcessHeader" $ do
    it "post-processes given license header using given configuration" $ do
      let header = fromLines
            [ "License header"
            , "Copyright (c) 2019 1st Author"
            , "Copyright (c) 2018-2019 2nd Author"
            ]
          expected = fromLines
            [ "License header"
            , "Copyright (c) 2019 1st Author"
            , "Copyright (c) 2018-2020 2nd Author"
            ]
          env = configuredEnv "2nd Author"
      postProcessHeader env header `shouldBe` expected


  describe "mkConfiguredEnv" $ do
    it "makes ConfiguredEnv from input parameters" $ do
      let configsIn = configs "{{ sndAuthor }}"
          envOut    = configuredEnv "2nd Author"
      mkConfiguredEnv currentYear vars configsIn `shouldBe` Just envOut

-------------------------------  Test Data Types  ------------------------------

newtype TestEnv = TestEnv Text

instance Has TestEnv TestEnv where
  hasLens = id

testFn :: (Has TestEnv env) => HeaderFn env
testFn = HeaderFn $ \input -> do
  TestEnv text <- viewL
  pure $ input <> "_" <> text
