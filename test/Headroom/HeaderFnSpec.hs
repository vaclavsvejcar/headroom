{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Headroom.HeaderFnSpec
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
  describe "runHeaderFn" $ do
    it "executes the function for given environment" $ do
      let testEnv  = TestEnv "ENV"
          input    = "input"
          actual   = runHeaderFn testFn testEnv input
          expected = "input_ENV"
      actual `shouldBe` expected

-------------------------------  Test Data Types  ------------------------------

newtype TestEnv = TestEnv Text

instance Has TestEnv TestEnv where
  hasLens = id

testFn :: (Has TestEnv env) => HeaderFn env
testFn = HeaderFn $ \input -> do
  TestEnv text <- viewL
  pure $ input <> "_" <> text
