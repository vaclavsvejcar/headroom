{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.SerializationSpec
  ( spec
  )
where

import           RIO
import           Headroom.Serialization
import           Test.Hspec


spec :: Spec
spec = do
  describe "dropFieldPrefix" $ do
    it "removes prefix and lowercases first letter for 'prSomeField'" $ do
      dropFieldPrefix "prSomeField" `shouldBe` "someField"

    it "removes prefix and keeps case for 'prURLField'" $ do
      dropFieldPrefix "prURLField" `shouldBe` "URLField"

  describe "symbolCase" $ do
    it "replaces camel cased string into symbol cased" $ do
      let input    = "camelCasedValue"
          expected = "camel-cased-value"
      symbolCase '-' input `shouldBe` expected
