{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Data.EnumExtraSpec
    ( spec
    )
where

import Headroom.Data.EnumExtra
import RIO
import Test.Hspec

data TestEnum
    = Foo
    | Bar
    deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)

spec :: Spec
spec = do
    describe "allValues" $ do
        it "should return list of all enum values" $ do
            allValues @TestEnum `shouldBe` [Foo, Bar]

    describe "allValuesToText" $ do
        it "should pretty print all enum values" $ do
            allValuesToText @TestEnum `shouldBe` "Foo, Bar"

    describe "enumToText" $ do
        it "should show textual representation of enum value" $ do
            enumToText Foo `shouldBe` "Foo"

    describe "textToEnum" $ do
        it "should read enum value from textual representation" $ do
            textToEnum "foo" `shouldBe` Just Foo
