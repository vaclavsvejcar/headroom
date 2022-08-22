{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Data.CoerceSpec
    ( spec
    )
where

import Headroom.Data.Coerce
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "inner" $ do
        it "applies function to inner value of newtype" $ do
            let sample = Foo "hello"
                expected = Foo "HELLO"
            inner T.toUpper sample `shouldBe` expected

newtype Foo = Foo Text deriving (Eq, Show)
