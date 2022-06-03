{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Data.SerializationSpec (
    spec
) where

import Headroom.Data.Serialization
import RIO
import qualified RIO.Map as M
import Test.Hspec

spec :: Spec
spec = do
    describe "dropFieldPrefix" $ do
        it "removes prefix and lowercases first letter for 'prSomeField'" $ do
            dropFieldPrefix "prSomeField" `shouldBe` "someField"

        it "removes prefix and keeps case for 'prURLField'" $ do
            dropFieldPrefix "prURLField" `shouldBe` "URLField"

    describe "symbolCase" $ do
        it "replaces camel cased string into symbol cased" $ do
            let input = "camelCasedValue"
                expected = "camel-cased-value"
            symbolCase '-' input `shouldBe` expected

    describe "prettyPrintYAML" $ do
        it "pretty prints YAML" $ do
            let input = M.fromList [("foo" :: Text, ["bar"] :: [Text])]
                expected = "foo:\n- bar\n"
            prettyPrintYAML input `shouldBe` expected
