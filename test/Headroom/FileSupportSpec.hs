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

  describe "addHeader" $ do
    let fileInfo config = FileInfo Haskell config Nothing HM.empty

    it "adds header at the beginning of text" $ do
      let info     = fileInfo $ HeaderConfig ["hs"] [] [] "{-|" "-}"
          header   = "HEADER"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "HEADER\n1\n2\nbefore\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "adds header after 'putAfter' position" $ do
      let info     = fileInfo $ HeaderConfig ["hs"] ["^before"] [] "{-|" "-}"
          header   = "{-| HEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| HEADER -}\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "does nothing if header is already present" $ do
      let config = HeaderConfig ["hs"] ["^before"] [] "{-|" "-}"
          header = "{-| HEADER -}"
          info   = FileInfo Haskell config (Just (3, 3)) HM.empty
          sample = "1\n2\nbefore\n{-| OLDHEADER -}\nafter\n4"
      addHeader info header sample `shouldBe` sample


  describe "dropHeader" $ do
    it "does nothing if no header is present" $ do
      let config = HeaderConfig ["hs"] [] [] "{-|" "-}"
          info   = FileInfo Haskell config Nothing HM.empty
          sample = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` sample

    it "drops existing single line header" $ do
      let config   = HeaderConfig ["hs"] [] [] "{-|" "-}"
          info     = FileInfo Haskell config (Just (3, 3)) HM.empty
          sample   = "1\n2\nbefore\n{-| HEADER -}\nafter\n4\n"
          expected = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` expected

    it "drops existing multi line header" $ do
      let config   = HeaderConfig ["hs"] [] [] "{-|" "-}"
          info     = FileInfo Haskell config (Just (3, 4)) HM.empty
          sample   = "1\n2\nbefore\n{-| HEADER\nHERE -}\nafter\n4\n"
          expected = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` expected


  describe "replaceHeader" $ do
    it "adds header if there's none present" $ do
      let config   = HeaderConfig ["hs"] ["^before"] [] "{-|" "-}"
          info     = FileInfo Haskell config Nothing HM.empty
          header   = "{-| NEWHEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| NEWHEADER -}\nafter\n4\n"
      replaceHeader info header sample `shouldBe` expected

    it "replaces header if there's existing one" $ do
      let config   = HeaderConfig ["hs"] ["^before"] [] "{-|" "-}"
          info     = FileInfo Haskell config (Just (3, 4)) HM.empty
          header   = "{-| NEWHEADER -}"
          sample   = "1\n2\nbefore\n{-| OLD\nHEADER -}\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| NEWHEADER -}\nafter\n4\n"
      replaceHeader info header sample `shouldBe` expected


  describe "extractFileInfo" $ do
    it "extracts FileInfo from given raw input" $ do
      let config   = HeaderConfig ["hs"] [] [] "{-|" "-}"
          expected = FileInfo Haskell config (Just (1, 13)) HM.empty
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractFileInfo Haskell config sample `shouldBe` expected


  describe "findHeader" $ do
    it "finds single line header" $ do
      let sample = "\n{-| single line -}\n\n"
          config = HeaderConfig ["hs"] [] [] "{-|" "-}"
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds header put after given regex" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = HeaderConfig ["hs"] ["^foo"] [] "{-|" "-}"
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds header put after composed regex" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = HeaderConfig ["hs"] ["^bar", "^foo"] [] "{-|" "-}"
      findHeader config sample `shouldBe` Just (5, 6)

    it "finds header in longer example" $ do
      let config = HeaderConfig ["hs"] [] [] "{-|" "-}"
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      findHeader config sample `shouldBe` Just (1, 13)

    it "finds nothing if no header present" $ do
      let sample = "some\nrandom\text without header"
          config = HeaderConfig ["hs"] [] [] "{-|" "-}"
      findHeader config sample `shouldBe` Nothing

    it "finds nothing if header is present before the 'putAfter' settings" $ do
      let sample = "foo\n{-| 1 -}\nbar\nsome text"
          config = HeaderConfig ["hs"] ["^bar"] [] "{-|" "-}"
      findHeader config sample `shouldBe` Nothing


  describe "findPrefixedHeader" $ do
    it "finds single line header" $ do
      let sample = ["-- foo", ""]
      findPrefixedHeader "--" sample 0 `shouldBe` Just (0, 0)

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
