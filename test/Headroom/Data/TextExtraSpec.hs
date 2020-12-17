{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Data.TextExtraSpec
  ( spec
  )
where

import           Headroom.Data.TextExtra
import           RIO
import qualified RIO.Text                           as T
import           Test.Hspec


spec :: Spec
spec = do

  describe "commonLinesPrefix" $ do
    it "returns longest common prefix for lines of text" $ do
      let text = fromLines ["-- foo", "-- bar", "-- xx"]
      commonLinesPrefix text `shouldBe` Just "-- "

    it "returns Nothing if not all elements starts with same prefix" $ do
      let text = fromLines ["-- foo", "bar", "-- xx"]
      commonLinesPrefix text `shouldBe` Nothing

    it "returns Nothing if no common prefix found" $ do
      let text = fromLines ["foo", "bar", "hello"]
      commonLinesPrefix text `shouldBe` Nothing

    it "returns Nothing if input is empty" $ do
      commonLinesPrefix T.empty `shouldBe` Nothing


  describe "mapLines" $ do
    it "returns same output for identity function" $ do
      let sample = fromLines ["foo zz", "bar", "xx"]
      mapLines id sample `shouldBe` sample

    it "maps all lines using the function" $ do
      let sample   = fromLines ["foo zz", "bar", "xx"]
          fn       = ("L: " <>)
          expected = fromLines ["L: foo zz", "L: bar", "L: xx"]
      mapLines fn sample `shouldBe` expected


  describe "mapLinesF" $ do
    it "returns same output for identity function" $ do
      let sample = fromLines ["foo zz", "bar", "xx"]
      mapLinesF (Just <$> id) sample `shouldBe` sample

    it "maps all lines using the function" $ do
      let sample   = fromLines ["foo zz", "bar", "xx"]
          fn       = \l -> if l == "bar" then Nothing else Just ("L: " <> l)
          expected = fromLines ["L: foo zz", "L: xx"]
      mapLinesF fn sample `shouldBe` expected


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
