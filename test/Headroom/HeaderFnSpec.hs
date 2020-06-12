{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Headroom.HeaderFnSpec
  ( spec
  )
where

import           Headroom.Configuration.Types   ( HeaderFnConfig'(..)
                                                , HeaderFnConfigs'(..)
                                                , UpdateCopyrightConfig'(..)
                                                )
import           Headroom.Data.Has              ( Has(..) )
import           Headroom.HeaderFn
import           Headroom.HeaderFn.Types
import           Headroom.HeaderFn.UpdateCopyright
import           Headroom.Types                 ( CurrentYear(..) )
import           RIO
import qualified RIO.Text                      as T
import           Test.Hspec


spec :: Spec
spec = do
  let currentYear = CurrentYear 2020
      mode        = UpdateSelectedAuthors . SelectedAuthors $ "2nd Author" :| []
      configs     = HeaderFnConfigs
        { hfcsUpdateCopyright = HeaderFnConfig
                                  { hfcEnabled = True
                                  , hfcConfig  = UpdateCopyrightConfig
                                                   { uccSelectedAuthors =
                                                     Just $ "2nd Author" :| []
                                                   }
                                  }
        }
      configuredEnv = ConfiguredEnv { ceCurrentYear         = currentYear
                                    , ceHeaderFnConfigs     = configs
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
    it "postprocesses given license header using given configuration" $ do
      let header = T.unlines
            [ "License header"
            , "Copyright (c) 2019 1st Author"
            , "Copyright (c) 2018-2019 2nd Author"
            ]
          expected = T.unlines
            [ "License header"
            , "Copyright (c) 2019 1st Author"
            , "Copyright (c) 2018-2020 2nd Author"
            ]
      postProcessHeader configuredEnv header `shouldBe` expected


  describe "mkConfiguredEnv" $ do
    it "makes ConfiguredEnv from input parameters" $ do
      mkConfiguredEnv currentYear configs `shouldBe` configuredEnv

-------------------------------  Test Data Types  ------------------------------

newtype TestEnv = TestEnv Text

instance Has TestEnv TestEnv where
  hasLens = id

testFn :: (Has TestEnv env) => HeaderFn env
testFn = HeaderFn $ \input -> do
  TestEnv text <- viewL
  pure $ input <> "_" <> text
