{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Data.TextExtraSpec
  ( spec
  )
where

import           Headroom.Data.TextExtra
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "mapLines" $ do
    it "should return same output for identity function" $ do
      let sample = fromLines ["foo zz", "bar", "xx"]
      mapLines id sample `shouldBe` sample

    it "should map all lines using the function" $ do
      let sample   = fromLines ["foo zz", "bar", "xx"]
          fn       = ("L: " <>)
          expected = fromLines ["L: foo zz", "L: bar", "L: xx"]
      mapLines fn sample `shouldBe` expected


  describe "read" $ do
    it "parses value from given text using Read instance" $ do
      read @Int "123" `shouldBe` Just 123


  describe "fromLines" $ do
    it "handles correctly empty linex" $ do
      fromLines [] `shouldBe` ""

    it "handles correctly single line text" $ do
      fromLines ["foo"] `shouldBe` "foo"

    it "joins lines of text" $ do
      fromLines ["first", "second"] `shouldBe` "first\nsecond"
      fromLines ["first", "second", ""] `shouldBe` "first\nsecond\n"


  describe "toLines" $ do
    it "handles correctly empty text" $ do
      toLines "" `shouldBe` []

    it "splits lines of text to list" $ do
      toLines "first\nsecond" `shouldBe` ["first", "second"]
      toLines "first\nsecond\n" `shouldBe` ["first", "second", ""]


  describe "toLines . fromLines" $ do
    it "does not alter newlines in processed text" $ do
      (fromLines . toLines $ "first\nsecond") `shouldBe` "first\nsecond"
      (fromLines . toLines $ "first\nsecond\n") `shouldBe` "first\nsecond\n"
