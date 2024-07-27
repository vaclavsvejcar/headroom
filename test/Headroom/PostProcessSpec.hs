{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.PostProcessSpec
    ( spec
    )
where

import Headroom.Config.Types
    ( PostProcessConfig (..)
    , PostProcessConfigs (..)
    , UpdateCopyrightConfig (..)
    )
import Headroom.Data.Has (Has (..))
import Headroom.Data.Text (fromLines)
import Headroom.PostProcess
import Headroom.PostProcess.Types
import Headroom.PostProcess.UpdateCopyright
import Headroom.Template.Mustache (Mustache)
import Headroom.Types (CurrentYear (..))
import Headroom.Variables (mkVariables)
import RIO
import Test.Hspec

spec :: Spec
spec = do
    let currentYear = CurrentYear 2020
        mode = UpdateSelectedAuthors . SelectedAuthors $ "2nd Author" :| []
        vars = mkVariables [("sndAuthor", "2nd Author")]
        configs a =
            PostProcessConfigs
                { ppcsUpdateCopyright =
                    PostProcessConfig
                        { ppcEnabled = True
                        , ppcConfig =
                            UpdateCopyrightConfig
                                { uccSelectedAuthors =
                                    Just
                                        $ a
                                        :| []
                                }
                        }
                }
        configuredEnv a =
            ConfiguredEnv
                { ceCurrentYear = currentYear
                , cePostProcessConfigs = configs a
                , ceUpdateCopyrightMode = mode
                }

    describe "postProcess" $ do
        it "executes the function for given environment" $ do
            let testEnv = TestEnv "ENV"
                input = "input"
                expected = "input_ENV"
            postProcess testFn testEnv input `shouldBe` expected

    describe "postProcessHeader" $ do
        it "post-processes license header using given configuration" $ do
            let header =
                    fromLines
                        [ "License header"
                        , "Copyright (c) 2019 1st Author"
                        , "Copyright (c) 2018-2019 2nd Author"
                        ]
                expected =
                    fromLines
                        [ "License header"
                        , "Copyright (c) 2019 1st Author"
                        , "Copyright (c) 2018-2020 2nd Author"
                        ]
                env = configuredEnv "2nd Author"
            postProcessHeader env header `shouldBe` expected

    describe "mkConfiguredEnv" $ do
        it "makes ConfiguredEnv from input parameters" $ do
            let configsIn = configs "{{ sndAuthor }}"
                out = configuredEnv "2nd Author"
            mkConfiguredEnv @Mustache currentYear vars configsIn `shouldBe` Just out

-------------------------------  Test Data Types  ------------------------------

newtype TestEnv = TestEnv Text

instance Has TestEnv TestEnv where
    hasLens = id

testFn :: (Has TestEnv env) => PostProcess env
testFn = PostProcess $ \input -> do
    TestEnv text <- viewL
    pure $ input <> "_" <> text
