{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Headroom.FileSupportSpec
  ( spec
  )
where

import           Headroom.Configuration         ( makeHeadersConfig
                                                , parseConfiguration
                                                )
import           Headroom.Embedded              ( defaultConfig )
import           Headroom.FileSupport
import           Headroom.FileSystem            ( loadFile )
import           Headroom.Types                 ( FileInfo(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                , HeadersConfig(..)
                                                , PartialConfiguration(..)
                                                )
import           Headroom.Variables             ( mkVariables )
import           RIO
import           RIO.FilePath                   ( (</>) )
import           Test.Hspec
import           Text.Regex.PCRE.Light          ( compile )
import           Text.Regex.PCRE.Light.Char8    ( utf8 )


spec :: Spec
spec = do
  let samplesDir = "test-data" </> "code-samples"
      lHeaderConfig pb pa = HeaderConfig ["hs"] 0 0 pb pa (LineComment "--")
      bHeaderConfig = bHeaderConfigM 0 0
      bHeaderConfigM mb ma pb pa =
        HeaderConfig ["hs"] mb ma pb pa (BlockComment "{-|" "-}")

  describe "addHeader" $ do
    let fileInfo config = FileInfo Haskell config Nothing mempty

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
          info   = FileInfo Haskell config (Just (3, 3)) mempty
          sample = "1\n2\nbefore\n{-| OLDHEADER -}\nafter\n4"
      addHeader info header sample `shouldBe` sample


  describe "dropHeader" $ do
    it "does nothing if no header is present" $ do
      let config = bHeaderConfig [] []
          info   = FileInfo Haskell config Nothing mempty
          sample = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` sample

    it "drops existing single line header" $ do
      let config   = bHeaderConfig [] []
          info     = FileInfo Haskell config (Just (3, 3)) mempty
          sample   = "1\n2\nbefore\n{-| HEADER -}\nafter\n4\n"
          expected = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` expected

    it "drops existing multi line header" $ do
      let config   = bHeaderConfig [] []
          info     = FileInfo Haskell config (Just (3, 4)) mempty
          sample   = "1\n2\nbefore\n{-| HEADER\nHERE -}\nafter\n4\n"
          expected = "1\n2\nbefore\nafter\n4\n"
      dropHeader info sample `shouldBe` expected


  describe "replaceHeader" $ do
    it "adds header if there's none present" $ do
      let config   = bHeaderConfig ["^before"] []
          info     = FileInfo Haskell config Nothing mempty
          header   = "{-| NEWHEADER -}"
          sample   = "1\n2\nbefore\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| NEWHEADER -}\nafter\n4\n"
      replaceHeader info header sample `shouldBe` expected

    it "replaces header if there's existing one" $ do
      let config   = bHeaderConfig ["^before"] []
          info     = FileInfo Haskell config (Just (3, 4)) mempty
          header   = "{-| NEWHEADER -}"
          sample   = "1\n2\nbefore\n{-| OLD\nHEADER -}\nafter\n4\n"
          expected = "1\n2\nbefore\n{-| NEWHEADER -}\nafter\n4\n"
      replaceHeader info header sample `shouldBe` expected


  describe "extractFileInfo" $ do
    it "extracts FileInfo from given raw input" $ do
      let config   = bHeaderConfig [] []
          expected = FileInfo
            Haskell
            config
            (Just (1, 13))
            (mkVariables
              [ ( "_haskell_module_copyright"
                , "(c) Some Guy, 2013\n                  Someone Else, 2014"
                )
              , ("_haskell_module_name"     , "Test")
              , ("_haskell_module_longdesc" , "long\ndescription")
              , ("_haskell_module_shortdesc", "Short description")
              ]
            )
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractFileInfo Haskell config sample `shouldBe` expected

  describe "extractVariables" $ do
    it "extracts variables specific for Haskell file type" $ do
      let config   = bHeaderConfig [] []
          expected = mkVariables
            [ ( "_haskell_module_copyright"
              , "(c) Some Guy, 2013\n                  Someone Else, 2014"
              )
            , ("_haskell_module_name"     , "Test")
            , ("_haskell_module_longdesc" , "long\ndescription")
            , ("_haskell_module_shortdesc", "Short description")
            ]
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractVariables Haskell config (Just (1, 13)) sample `shouldBe` expected


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

    it "correctly detects headers using default YAML configuration" $ do
      let path = "test-data" </> "code-samples"
      defaultConfig'     <- parseConfiguration defaultConfig
      HeadersConfig {..} <- makeHeadersConfig (pcLicenseHeaders defaultConfig')
      sampleC1           <- loadFile $ path </> "c" </> "sample1.c"
      sampleC2           <- loadFile $ path </> "c" </> "sample2.c"
      sampleCpp1         <- loadFile $ path </> "cpp" </> "sample1.cpp"
      sampleCpp2         <- loadFile $ path </> "cpp" </> "sample2.cpp"
      sampleCss1         <- loadFile $ path </> "css" </> "sample1.css"
      sampleCss2         <- loadFile $ path </> "css" </> "sample2.css"
      sampleHs1          <- loadFile $ path </> "haskell" </> "sample1.hs"
      sampleHs2          <- loadFile $ path </> "haskell" </> "sample2.hs"
      sampleHtml1        <- loadFile $ path </> "html" </> "sample1.html"
      sampleHtml2        <- loadFile $ path </> "html" </> "sample2.html"
      sampleJava1        <- loadFile $ path </> "java" </> "sample1.java"
      sampleJava2        <- loadFile $ path </> "java" </> "sample2.java"
      sampleJs1          <- loadFile $ path </> "js" </> "sample1.js"
      sampleRust1        <- loadFile $ path </> "rust" </> "sample1.rs"
      sampleScala1       <- loadFile $ path </> "scala" </> "sample1.scala"
      sampleScala2       <- loadFile $ path </> "scala" </> "sample2.scala"
      sampleShell1       <- loadFile $ path </> "shell" </> "sample1.sh"
      findHeader hscC sampleC1 `shouldBe` Just (1, 3)
      findHeader hscC sampleC2 `shouldBe` Nothing
      findHeader hscCpp sampleCpp1 `shouldBe` Just (1, 3)
      findHeader hscCpp sampleCpp2 `shouldBe` Nothing
      findHeader hscCss sampleCss1 `shouldBe` Just (1, 4)
      findHeader hscCss sampleCss2 `shouldBe` Nothing
      findHeader hscHaskell sampleHs1 `shouldBe` Just (1, 3)
      findHeader hscHaskell sampleHs2 `shouldBe` Nothing
      findHeader hscHtml sampleHtml1 `shouldBe` Just (1, 4)
      findHeader hscHtml sampleHtml2 `shouldBe` Nothing
      findHeader hscJava sampleJava1 `shouldBe` Just (0, 2)
      findHeader hscJava sampleJava2 `shouldBe` Nothing
      findHeader hscJs sampleJs1 `shouldBe` Just (0, 2)
      findHeader hscRust sampleRust1 `shouldBe` Just (0, 2)
      findHeader hscScala sampleScala1 `shouldBe` Just (0, 2)
      findHeader hscScala sampleScala2 `shouldBe` Nothing
      findHeader hscShell sampleShell1 `shouldBe` Just (2, 3)


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

