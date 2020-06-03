{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Headroom.HeaderFn.TypesSpec
  ( spec
  )
where

import           Headroom.Data.Has              ( Has(..) )
import           Headroom.HeaderFn              ( runHeaderFn )
import           Headroom.HeaderFn.Types
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "Semigroup HeaderFn" $ do
    it "combines two values together" $ do
      let fooEnv      = FooEnv "_FOO_ENV"
          barEnv      = BarEnv "_BAR_ENV"
          combinedEnv = CombinedEnv fooEnv barEnv
          input       = "input"
          combinedFn  = fooFn <> barFn
          expected    = "input_FOO_ENV_BAR_ENV"
          actual      = runHeaderFn combinedFn combinedEnv input
      actual `shouldBe` expected

  describe "Monoid HeaderFn" $ do
    it "does nothing with the input" $ do
      let input   = "input"
          testEnv = undefined
          testFn  = mempty
          actual  = runHeaderFn testFn testEnv input
      actual `shouldBe` input


-------------------------------  Test Data Types  ------------------------------


data FooEnv = FooEnv
  { feValue :: !Text
  }

data BarEnv = BarEnv
  { beValue :: !Text
  }

data CombinedEnv = CombinedEnv
  { ceFooEnv :: !FooEnv
  , ceBarEnv :: !BarEnv
  }


instance Has FooEnv FooEnv where
  hasLens = id

instance Has FooEnv CombinedEnv where
  hasLens = lens ceFooEnv (\x y -> x { ceFooEnv = y })

instance Has BarEnv CombinedEnv where
  hasLens = lens ceBarEnv (\x y -> x { ceBarEnv = y })


fooFn :: (Has FooEnv env) => HeaderFn env
fooFn = HeaderFn $ \input -> do
  FooEnv {..} <- viewL
  pure $ input <> feValue

barFn :: (Has BarEnv env) => HeaderFn env
barFn = HeaderFn $ \input -> do
  BarEnv {..} <- viewL
  pure $ input <> beValue
