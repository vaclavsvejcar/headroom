{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.RegexSpec
  ( spec
  )
where

import           Headroom.Regex
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "joinPatterns" $ do
    it "returns Nothing for empty input" $ do
      joinPatterns [] `shouldBe` Nothing

    it "handles single item list input" $ do
      joinPatterns ["foo"] `shouldBe` Just "foo"

    it "handles multiple item list input" $ do
      joinPatterns ["^foo", "^bar"] `shouldBe` Just "^foo|^bar"


  describe "match'" $ do
    it "matches regular expression against given sample" $ do
      let regex = compile' <$> joinPatterns ["foo", "bar"]
      (regex >>= (`match'` "xxx")) `shouldSatisfy` isNothing
      (regex >>= (`match'` "foz")) `shouldSatisfy` isNothing
      (regex >>= (`match'` "foosdas")) `shouldSatisfy` isJust
      (regex >>= (`match'` "barfoo")) `shouldSatisfy` isJust
