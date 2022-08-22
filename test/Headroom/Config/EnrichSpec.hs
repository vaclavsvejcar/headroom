{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Config.EnrichSpec
    ( spec
    )
where

import Headroom.Config.Enrich
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "withArray" $ do
        it "produces valid YAML array field" $ do
            let field = "name"
                values = ["foo", "bar"] :: [Text]
            withArray values field `shouldBe` (Array, "name:\n- foo\n- bar")

    describe "withText" $ do
        it "produces valid YAML string from Text value" $ do
            let field = "name"
                value = "John Smith" :: Text
            withText value field `shouldBe` (String, "name: John Smith")

    describe "replaceEmptyValue" $ do
        it "replaces YAML array field" $ do
            let field = "name"
                values = ["foo", "bar"] :: [Text]
                yaml = "name: []"
                action = replaceEmptyValue field $ withArray values
            enrich action yaml `shouldBe` "name:\n- foo\n- bar"

        it "replaces multiple YAML array fields using Semigroup instance" $ do
            let animalsF = "animals"
                animals = ["dog", "cat"] :: [Text]
                colorsF = "colors"
                colors = ["blue", "red"] :: [Text]
                yaml = "colors: []\nanimals: []"
                enrichAnimals = replaceEmptyValue animalsF $ withArray animals
                enrichColors = replaceEmptyValue colorsF (withArray colors)
                action = enrichAnimals <> enrichColors
                expected = "colors:\n- blue\n- red\nanimals:\n- dog\n- cat"
            enrich action yaml `shouldBe` expected
