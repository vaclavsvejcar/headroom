{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileSupportSpec
  ( spec
  )
where

import           Headroom.FileSupport
import           Headroom.Types                 ( FileInfo(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashMap                   as HM
import           Test.Hspec
import           Text.Regex.PCRE.Light          ( compile )
import           Text.Regex.PCRE.Light.Char8    ( utf8 )


spec :: Spec
spec = do
  let samplesDir = "test-data" </> "code-samples"
      bHeaderConfig b a = HeaderConfig ["hs"] b a (BlockHeader "{-|" "-}")
      pHeaderConfig b a = HeaderConfig ["hs"] b a (PrefixedHeader "--")

  describe "addHeader" $ do
    let fileInfo config = FileInfo Haskell config Nothing HM.empty

    it "adds header at the beginning of text" $ do
      let info     = fileInfo $ bHeaderConfig [] []
          header   = "HEADER"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "HEADER\n1\n2\nbefore\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "adds header after 'putAfter' position" $ do
      let info     = fileInfo $ bHeaderConfig ["^before"] []
          header   = "{-| HEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| HEADER -}\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "does nothing if header is already present" $ do
      let config = bHeaderConfig ["^before"] []
          header = "{-| HEADER -}"
          info   = FileInfo Haskell config (Just (3, 3)) HM.empty
          sample = "1\n2\nbefore\n{-| OLDHEADER -}\nafter\n4"
      addHeader info header sample `shouldBe` sample


  describe "dropHeader" $ do
    it "does nothing if no header is present" $ do
      let config = bHeaderConfig [] []
          info   = FileInfo Haskell config Nothing HM.empty
          sample = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` sample

    it "drops existing single line header" $ do
      let config   = bHeaderConfig [] []
          info     = FileInfo Haskell config (Just (3, 3)) HM.empty
          sample   = "1\n2\nbefore\n{-| HEADER -}\nafter\n4\n"
          expected = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` expected

    it "drops existing multi line header" $ do
      let config   = bHeaderConfig [] []
          info     = FileInfo Haskell config (Just (3, 4)) HM.empty
          sample   = "1\n2\nbefore\n{-| HEADER\nHERE -}\nafter\n4\n"
          expected = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` expected


  describe "replaceHeader" $ do
    it "adds header if there's none present" $ do
      let config   = bHeaderConfig ["^before"] []
          info     = FileInfo Haskell config Nothing HM.empty
          header   = "{-| NEWHEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| NEWHEADER -}\nafter\n4\n"
      replaceHeader info header sample `shouldBe` expected

    it "replaces header if there's existing one" $ do
      let config   = bHeaderConfig ["^before"] []
          info     = FileInfo Haskell config (Just (3, 4)) HM.empty
          header   = "{-| NEWHEADER -}"
          sample   = "1\n2\nbefore\n{-| OLD\nHEADER -}\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| NEWHEADER -}\nafter\n4\n"
      replaceHeader info header sample `shouldBe` expected


  describe "extractFileInfo" $ do
    it "extracts FileInfo from given raw input" $ do
      let config   = bHeaderConfig [] []
          expected = FileInfo Haskell config (Just (1, 13)) HM.empty
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractFileInfo Haskell config sample `shouldBe` expected


  describe "findHeader" $ do
    it "finds single line block header" $ do
      let sample = "\n{-| single line -}\n\n"
          config = bHeaderConfig [] []
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds single line prefixed header" $ do
      let sample = "\n-- single line\n\n"
          config = pHeaderConfig [] []
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds block header put after 'putAfter' constraint" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = bHeaderConfig ["^foo"] []
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds prefixed header put after 'putAfter' constraint" $ do
      let sample = "-- 1\nfoo\n-- 2\n-- 2\nbar\n-- 3\n-- 3"
          config = pHeaderConfig ["^foo"] []
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds block header put after composed constraint" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = bHeaderConfig ["^bar", "^foo"] []
      findHeader config sample `shouldBe` Just (5, 6)

    it "finds prefixed header put after composed constraint" $ do
      let sample = "-- 1\nfoo\n-- 2\n-- 2\nbar\n-- 3\n-- 3"
          config = pHeaderConfig ["^bar", "^foo"] []
      findHeader config sample `shouldBe` Just (5, 6)

    it "finds nothing if no header present" $ do
      let sample = "some\nrandom\text without header"
          config = bHeaderConfig [] []
      findHeader config sample `shouldBe` Nothing

    it "finds nothing if header is present before 'putAfter' constraint" $ do
      let sample = "foo\n{-| 1 -}\nbar\nsome text"
          config = bHeaderConfig ["^bar"] []
      findHeader config sample `shouldBe` Nothing


  describe "findBlockHeader" $ do
    it "finds single line header" $ do
      let sample = ["", "{-| single line -}", "", ""]
      findBlockHeader "{-|" "-}" sample 0 `shouldBe` Just (1, 1)

    it "finds multi line header" $ do
      let sample = ["", "{-| multi", "line -}", "", ""]
      findBlockHeader "{-|" "-}" sample 0 `shouldBe` Just (1, 2)

    it "finds only the first occurence of header" $ do
      let sample = ["{-| this", "and this -}", "", "{-| no this -}"]
      findBlockHeader "{-|" "-}" sample 0 `shouldBe` Just (0, 1)

    it "finds nothing if no header is present" $ do
      let sample = ["foo", "bar"]
      findBlockHeader "{-|" "-}" sample 0 `shouldBe` Nothing


  describe "findPrefixedHeader" $ do
    it "finds single line header" $ do
      let sample = ["-- foo", ""]
      findPrefixedHeader "--" sample 0 `shouldBe` Just (0, 0)

    it "finds single line header with nothing surrounding it" $ do
      let sample = ["-- foo"]
      findPrefixedHeader "--" sample 0 `shouldBe` Just (0, 0)

    it "finds multi line header with nothing surrounding it" $ do
      let sample = ["-- 3", "-- 3"]
      findPrefixedHeader "--" sample 0 `shouldBe` Just (0, 1)

    it "finds multi line header" $ do
      let sample = ["", "a", "-- first", "--second", "foo"]
      findPrefixedHeader "--" sample 0 `shouldBe` Just (2, 3)

    it "finds only the first occurence of header" $ do
      let sample = ["a", "-- this one", "-- and this", "", "-- not this"]
      findPrefixedHeader "--" sample 0 `shouldBe` Just (1, 2)

    it "finds nothing if no header is present" $ do
      let sample = ["foo", "bar"]
      findPrefixedHeader "--" sample 0 `shouldBe` Nothing


  describe "lastMatching" $ do
    let regex = compile "^foo" [utf8]

    it "finds very last line that matches given regex" $ do
      let sample = ["some text", "hello", "foo bar", "foo baz", "last one"]
      lastMatching regex sample `shouldBe` 3

    it "returns 0 if the input is empty" $ do
      lastMatching regex [] `shouldBe` 0


  describe "firstMatching" $ do
    let regex = compile "^foo" [utf8]

    it "finds very first line that matches given regex" $ do
      let sample = ["some text", "hello", "foo bar", "foo baz", "last one"]
      firstMatching regex sample `shouldBe` 2

    it "returns 0 if the input is empty" $ do
      firstMatching regex [] `shouldBe` 0


  describe "splitInput" $ do
    let sample    = "text\n->\nRESULT\n<-\nfoo"
        putAfter  = ["->"]
        putBefore = ["<-"]

    it "handles empty input and conditions" $ do
      splitInput [] [] "" `shouldBe` ([], [], [])

    it "handles input and empty conditions" $ do
      splitInput [] [] "one\ntwo" `shouldBe` ([], ["one", "two"], [])

    it "splits input with 'putAfter' condition" $ do
      let expected = (["text", "->"], ["RESULT", "<-", "foo"], [])
      splitInput putAfter [] sample `shouldBe` expected

    it "splits input with 'putBefore' condition" $ do
      let expected = ([], ["text", "->", "RESULT"], ["<-", "foo"])
      splitInput [] putBefore sample `shouldBe` expected

    it "splits input with both conditions" $ do
      let expected = (["text", "->"], ["RESULT"], ["<-", "foo"])
      splitInput putAfter putBefore sample `shouldBe` expected
