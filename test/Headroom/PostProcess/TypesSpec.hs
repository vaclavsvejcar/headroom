{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.PostProcess.TypesSpec (
    spec
) where

import Headroom.Data.Has (Has (..))
import Headroom.PostProcess (postProcess)
import Headroom.PostProcess.Types
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "Semigroup PostProcess" $ do
        it "combines two values together" $ do
            let fooEnv = FooEnv "_FOO_ENV"
                barEnv = BarEnv "_BAR_ENV"
                combinedEnv = CombinedEnv fooEnv barEnv
                input = "input"
                combinedFn = fooFn <> barFn
                expected = "input_FOO_ENV_BAR_ENV"
            postProcess combinedFn combinedEnv input `shouldBe` expected

    describe "Monoid PostProcess" $ do
        it "does nothing with the input" $ do
            let input = "input"
                testEnv = undefined
                testFn = mempty
            postProcess testFn testEnv input `shouldBe` input

-------------------------------  Test Data Types  ------------------------------

data FooEnv = FooEnv
    { feValue :: Text
    }

data BarEnv = BarEnv
    { beValue :: Text
    }

data CombinedEnv = CombinedEnv
    { ceFooEnv :: FooEnv
    , ceBarEnv :: BarEnv
    }

instance Has FooEnv FooEnv where
    hasLens = id

instance Has FooEnv CombinedEnv where
    hasLens = lens ceFooEnv (\x y -> x{ceFooEnv = y})

instance Has BarEnv CombinedEnv where
    hasLens = lens ceBarEnv (\x y -> x{ceBarEnv = y})

fooFn :: (Has FooEnv env) => PostProcess env
fooFn = PostProcess $ \input -> do
    FooEnv{..} <- viewL
    pure $ input <> feValue

barFn :: (Has BarEnv env) => PostProcess env
barFn = PostProcess $ \input -> do
    BarEnv{..} <- viewL
    pure $ input <> beValue
