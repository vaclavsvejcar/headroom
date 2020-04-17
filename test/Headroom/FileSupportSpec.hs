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
      lHeaderConfig b a = HeaderConfig ["hs"] 0 0 b a (LineComment "--")
      bHeaderConfig pb pa = bHeaderConfigM 0 0 pb pa
      bHeaderConfigM mb ma pb pa =
        HeaderConfig ["hs"] mb ma pb pa (BlockComment "{-|" "-}")

  describe "addHeader" $ do
    let fileInfo config = FileInfo Haskell config Nothing HM.empty

    it "adds header at the beginning of text" $ do
      let info     = fileInfo $ bHeaderConfig [] []
          header   = "HEADER"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "HEADER\n1\n2\nbefore\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "adds header at the beginning of text (with correct margins)" $ do
      let info     = fileInfo $ bHeaderConfigM 2 2 [] []
          header   = "HEADER"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "HEADER\n\n\n1\n2\nbefore\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "adds header at correct position" $ do
      let info     = fileInfo $ bHeaderConfig ["^before"] ["^after"]
          header   = "{-| HEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| HEADER -}\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "adds header at correct position (with correct margins)" $ do
      let info     = fileInfo $ bHeaderConfigM 2 2 ["^before"] ["^after"]
          header   = "{-| HEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n\n\n{-| HEADER -}\n\n\nafter\n4\n"
      addHeader info header sample `shouldBe` expected

    it "adds header at the end of text (with correct margins)" $ do
      let info     = fileInfo $ bHeaderConfigM 2 2 ["^before"] []
          header   = "{-| HEADER -}"
          sample   = "1\n2\nbefore"
          expected = "1\n2\nbefore\n\n\n{-| HEADER -}\n"
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
    it "finds block header (one line long)" $ do
      let sample = "\n{-| single line -}\n\n"
          config = bHeaderConfig [] []
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds line header (one line long)" $ do
      let sample = "\n-- single line\n\n"
          config = lHeaderConfig [] []
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds block comment header put after 'putAfter' constraint" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = bHeaderConfig ["^foo"] []
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds line comment header put after 'putAfter' constraint" $ do
      let sample = "-- 1\nfoo\n-- 2\n-- 2\nbar\n-- 3\n-- 3"
          config = lHeaderConfig ["^foo"] []
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds block comment header put after composed constraint" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = bHeaderConfig ["^bar", "^foo"] []
      findHeader config sample `shouldBe` Just (5, 6)

    it "finds line comment header put after composed constraint" $ do
      let sample = "-- 1\nfoo\n-- 2\n-- 2\nbar\n-- 3\n-- 3"
          config = lHeaderConfig ["^bar", "^foo"] []
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


  describe "findLineHeader" $ do
    it "finds single line header" $ do
      let sample = ["-- foo", ""]
      findLineHeader "--" sample 0 `shouldBe` Just (0, 0)

    it "finds single line header with nothing surrounding it" $ do
      let sample = ["-- foo"]
      findLineHeader "--" sample 0 `shouldBe` Just (0, 0)

    it "finds multi line header with nothing surrounding it" $ do
      let sample = ["-- 3", "-- 3"]
      findLineHeader "--" sample 0 `shouldBe` Just (0, 1)

    it "finds multi line header" $ do
      let sample = ["", "a", "-- first", "--second", "foo"]
      findLineHeader "--" sample 0 `shouldBe` Just (2, 3)

    it "finds only the first occurence of header" $ do
      let sample = ["a", "-- this one", "-- and this", "", "-- not this"]
      findLineHeader "--" sample 0 `shouldBe` Just (1, 2)

    it "finds nothing if no header is present" $ do
      let sample = ["foo", "bar"]
      findLineHeader "--" sample 0 `shouldBe` Nothing


  describe "lastMatching" $ do
    let regex = compile "^foo" [utf8]

    it "finds very last line that matches given regex" $ do
      let sample = ["some text", "hello", "foo bar", "foo baz", "last one"]
      lastMatching regex sample `shouldBe` Just 3

    it "returns Nothing if no matching input found" $ do
      let sample = ["some text", "hello", "last one"]
      lastMatching regex sample `shouldBe` Nothing

    it "returns Nothing the input is empty" $ do
      lastMatching regex [] `shouldBe` Nothing


  describe "firstMatching" $ do
    let regex = compile "^foo" [utf8]

    it "finds very first line that matches given regex" $ do
      let sample = ["some text", "hello", "foo bar", "foo baz", "last one"]
      firstMatching regex sample `shouldBe` Just 2

    it "returns Nothing if the input is empty" $ do
      firstMatching regex [] `shouldBe` Nothing


  describe "splitInput" $ do
    let sample   = "text\n->\nRESULT\n<-\nfoo"
        fstSplit = ["->"]
        sndSplit = ["<-"]

    it "handles empty input and conditions" $ do
      splitInput [] [] "" `shouldBe` ([], [], [])

    it "handles input and empty conditions" $ do
      splitInput [] [] "one\ntwo" `shouldBe` ([], ["one", "two"], [])

    it "splits input with 1st split condition" $ do
      let expected = (["text", "->"], ["RESULT", "<-", "foo"], [])
      splitInput fstSplit [] sample `shouldBe` expected

    it "splits input with 2nd split condition" $ do
      let expected = ([], ["text", "->", "RESULT"], ["<-", "foo"])
      splitInput [] sndSplit sample `shouldBe` expected

    it "splits input with both conditions" $ do
      let expected = (["text", "->"], ["RESULT"], ["<-", "foo"])
      splitInput fstSplit sndSplit sample `shouldBe` expected

    it "splits input when nothing matches the 1st split condition" $ do
      let expected = ([], ["text", "RESULT", "<-", "foo"], [])
      splitInput fstSplit [] "text\nRESULT\n<-\nfoo" `shouldBe` expected

    it "splits input when nothing matches the 2nd split condition" $ do
      let expected = ([], ["text", "->", "RESULT", "foo"], [])
      splitInput [] sndSplit "text\n->\nRESULT\nfoo" `shouldBe` expected

    it "splits input when nothing matches both conditions" $ do
      let expected = ([], ["text", "RESULT", "foo"], [])
      splitInput fstSplit sndSplit "text\nRESULT\nfoo" `shouldBe` expected

    it "handles case when 2nd split is found before 1st split" $ do
      let expected = ([], ["text"], ["->", "RESULT", "<-", "foo"])
      splitInput sndSplit fstSplit sample `shouldBe` expected

    it "handles case when 1st split is also after 2nd split" $ do
      let expected = (["foo", "->"], ["RESULT"], ["<-", "bar", "->", "end"])
          sample'  = "foo\n->\nRESULT\n<-\nbar\n->\nend"
      splitInput fstSplit sndSplit sample' `shouldBe` expected

