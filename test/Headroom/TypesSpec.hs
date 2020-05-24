{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.TypesSpec
  ( spec
  )
where

import           Headroom.Types                 ( mkVariables )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "Semigroup Variables" $ do
    it "combines two instances of variables" $ do
      let sample1  = mkVariables [("fst", "v1"), ("snd", "v1")]
          sample2  = mkVariables [("snd", "v2"), ("trd", "v1")]
          expected = mkVariables [("trd", "v1"), ("snd", "v2"), ("fst", "v1")]
      (sample1 <> sample2) `shouldBe` expected
