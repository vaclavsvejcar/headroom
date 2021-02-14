{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Headroom.Header2Spec
  ( spec
  )
where

import           Headroom.Configuration              ( makeHeadersConfig
                                                     , parseConfiguration
                                                     )
import           Headroom.Configuration.Types        ( Configuration(..)
                                                     , HeaderConfig(..)
                                                     , HeaderSyntax(..)
                                                     , HeadersConfig(..)
                                                     , LicenseType(..)
                                                     )
import           Headroom.Data.Regex                 ( re )
import           Headroom.Embedded                   ( defaultConfig )
import           Headroom.FileSupport                ( analyzeSourceCode
                                                     , fileSupport
                                                     )
import           Headroom.FileSystem                 ( loadFile )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header2
import           Headroom.SourceCode                 ( LineType(..)
                                                     , SourceCode(..)
                                                     )
import           RIO
import           RIO.FilePath                        ( (</>) )
import           Test.Hspec                   hiding ( after
                                                     , before
                                                     )


spec :: Spec
spec = do

  let samplesDir = "test-data" </> "code-samples"
      lHeaderConfig pb pa =
        HeaderConfig ["hs"] 0 0 0 0 pb pa (LineComment [re|^--|] Nothing)
      bHeaderConfig = bHeaderConfigM 0 0 0 0
      bHeaderConfigM mtc mtf mbc mbf pb pa = HeaderConfig
        ["hs"]
        mtc
        mtf
        mbc
        mbf
        pb
        pa
        (BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing)


  describe "findHeader" $ do
    it "finds block header (one line long)" $ do
      let sample =
            SourceCode
              [ (Code   , "")
              , (Comment, "{-| single line -}")
              , (Code   , "")
              , (Code   , "")
              ]
          config = bHeaderConfig [] []
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds line header (one line long)" $ do
      let sample = SourceCode
            [(Code, ""), (Comment, "-- single line"), (Code, ""), (Code, "")]
          config = lHeaderConfig [] []
      findHeader config sample `shouldBe` Just (1, 1)

    it "finds block comment header put after 'putAfter' constraint" $ do
      let sample = SourceCode
            [ (Comment, "{-| 1 -}")
            , (Code   , "foo")
            , (Comment, "{-| 2")
            , (Comment, "2 -}")
            , (Code   , "bar")
            , (Comment, "{-| 3")
            , (Comment, "3 -}")
            ]
          config = bHeaderConfig [[re|^foo|]] []
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds line comment header put after 'putAfter' constraint" $ do
      let sample = SourceCode
            [ (Comment, "-- 1")
            , (Code   , "foo")
            , (Comment, "-- 2")
            , (Comment, "-- 2")
            , (Code   , "bar")
            , (Comment, "-- 3")
            , (Comment, "-- 3")
            ]
          config = lHeaderConfig [[re|^foo|]] []
      findHeader config sample `shouldBe` Just (2, 3)

    it "finds block comment header put after composed constraint" $ do
      let sample = SourceCode
            [ (Comment, "{-| 1 -}")
            , (Code   , "foo")
            , (Comment, "{-| 2")
            , (Comment, "2 -}")
            , (Code   , "bar")
            , (Comment, "{-| 3")
            , (Comment, "3 -}")
            ]
          config = bHeaderConfig [[re|^bar|^foo|]] []
      findHeader config sample `shouldBe` Just (5, 6)

    it "finds line comment header put after composed constraint" $ do
      let sample = SourceCode
            [ (Comment, "-- 1")
            , (Code   , "foo")
            , (Comment, "-- 2")
            , (Comment, "-- 2")
            , (Code   , "bar")
            , (Comment, "-- 3")
            , (Comment, "-- 3")
            ]
          config = lHeaderConfig [[re|^bar|^foo|]] []
      findHeader config sample `shouldBe` Just (5, 6)

    it "finds nothing if no header present" $ do
      let sample = SourceCode [(Code, "some"), (Code, "code without header")]
          config = bHeaderConfig [] []
      findHeader config sample `shouldBe` Nothing

    it "finds nothing if header is present before 'putAfter' constraint" $ do
      let sample = SourceCode
            [ (Code   , "foo")
            , (Comment, "{-| 1 -}")
            , (Code   , "bar")
            , (Code   , "some text")
            ]
          config = bHeaderConfig [[re|^bar|]] []
      findHeader config sample `shouldBe` Nothing

    it "correctly detects headers using default YAML configuration" $ do
      let loadSample = \ft p ->
            analyzeSourceCode (fileSupport ft) <$> loadFile (samplesDir </> p)
      defaultConfig'     <- parseConfiguration defaultConfig
      HeadersConfig {..} <- makeHeadersConfig (cLicenseHeaders defaultConfig')
      sampleC1           <- loadSample C $ "c" </> "sample1.c"
      sampleC2           <- loadSample C $ "c" </> "sample2.c"
      sampleCpp1         <- loadSample CPP $ "cpp" </> "sample1.cpp"
      sampleCpp2         <- loadSample CPP $ "cpp" </> "sample2.cpp"
      sampleCss1         <- loadSample CSS $ "css" </> "sample1.css"
      sampleCss2         <- loadSample CSS $ "css" </> "sample2.css"
      sampleHs1          <- loadSample Haskell $ "haskell" </> "sample1.hs"
      sampleHs2          <- loadSample Haskell $ "haskell" </> "sample2.hs"
      sampleHtml1        <- loadSample HTML $ "html" </> "sample1.html"
      sampleHtml2        <- loadSample HTML $ "html" </> "sample2.html"
      sampleJava1        <- loadSample Java $ "java" </> "sample1.java"
      sampleJava2        <- loadSample Java $ "java" </> "sample2.java"
      sampleJs1          <- loadSample JS $ "js" </> "sample1.js"
      sampleRust1        <- loadSample Rust $ "rust" </> "sample1.rs"
      sampleScala1       <- loadSample Scala $ "scala" </> "sample1.scala"
      sampleScala2       <- loadSample Scala $ "scala" </> "sample2.scala"
      sampleShell1       <- loadSample Shell $ "shell" </> "sample1.sh"
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
    let s = [re|^{-\||]
        e = [re|(?<!#)-}$|]

    it "finds single line header" $ do
      let sample =
            SourceCode
              [ (Code   , "")
              , (Comment, "{-| single line -}")
              , (Code   , "")
              , (Code   , "")
              ]
      findBlockHeader s e sample 0 `shouldBe` Just (1, 1)

    it "finds multi line header" $ do
      let sample = SourceCode
            [ (Code   , "")
            , (Comment, "{-| multi")
            , (Comment, "line -}")
            , (Code   , "")
            , (Code   , "")
            ]
      findBlockHeader s e sample 0 `shouldBe` Just (1, 2)

    it "finds only the first occurence of header" $ do
      let sample = SourceCode
            [ (Comment, "{-| this")
            , (Comment, "and this -}")
            , (Code   , "")
            , (Comment, "{-| no this -}")
            ]
      findBlockHeader s e sample 0 `shouldBe` Just (0, 1)

    it "finds nothing if no header is present" $ do
      let sample = SourceCode [(Code, "foo"), (Code, "bar")]
      findBlockHeader s e sample 0 `shouldBe` Nothing


  describe "findLineHeader" $ do
    it "finds single line header" $ do
      let sample = SourceCode [(Comment, "-- foo"), (Code, "other")]
      findLineHeader [re|^--|] sample 0 `shouldBe` Just (0, 0)

    it "finds single line header with nothing surrounding it" $ do
      let sample = SourceCode [(Comment, "-- foo")]
      findLineHeader [re|^--|] sample 0 `shouldBe` Just (0, 0)

    it "finds multi line header with nothing surrounding it" $ do
      let sample = SourceCode [(Comment, "-- 3"), (Comment, "-- 3")]
      findLineHeader [re|^--|] sample 0 `shouldBe` Just (0, 1)

    it "finds multi line header with added offset" $ do
      let sample = SourceCode [(Comment, "-- 3"), (Comment, "-- 3")]
      findLineHeader [re|^--|] sample 2 `shouldBe` Just (2, 3)

    it "finds multi line header" $ do
      let sample = SourceCode
            [ (Code   , "")
            , (Code   , "a")
            , (Comment, "-- first")
            , (Comment, "--second")
            , (Code   , "foo")
            ]
      findLineHeader [re|^--|] sample 0 `shouldBe` Just (2, 3)

    it "finds only the first occurence of header" $ do
      let sample = SourceCode
            [ (Code   , "a")
            , (Comment, "-- this one")
            , (Comment, "-- and this")
            , (Code   , "")
            , (Comment, "-- not this")
            ]
      findLineHeader [re|^--|] sample 0 `shouldBe` Just (1, 2)

    it "finds nothing if no header is present" $ do
      let sample = SourceCode [(Code, "foo"), (Code, "bar")]
      findLineHeader [re|^--|] sample 0 `shouldBe` Nothing


  describe "splitSource" $ do
    let sample = SourceCode
          [ (Code   , "some code")
          , (Comment, "->")
          , (Code   , "->")
          , (Code   , "RESULT")
          , (Comment, "<-")
          , (Code   , "<-")
          , (Code   , "also some code")
          ]
        fstSplit = [[re|->|]]
        sndSplit = [[re|<-|]]

    it "handles empty source code and conditions" $ do
      splitSource [] [] mempty `shouldBe` (mempty, mempty, mempty)

    it "handles source code and empty conditions" $ do
      splitSource [] [] sample `shouldBe` (mempty, sample, mempty)

    it "splits source code with 1st split condition" $ do
      let before =
            SourceCode [(Code, "some code"), (Comment, "->"), (Code, "->")]
          middle = SourceCode
            [ (Code   , "RESULT")
            , (Comment, "<-")
            , (Code   , "<-")
            , (Code   , "also some code")
            ]
          after    = mempty
          expected = (before, middle, after)
      splitSource fstSplit [] sample `shouldBe` expected

    it "splits source code with 2nd split condition" $ do
      let before = mempty
          middle = SourceCode
            [ (Code   , "some code")
            , (Comment, "->")
            , (Code   , "->")
            , (Code   , "RESULT")
            , (Comment, "<-")
            ]
          after    = SourceCode [(Code, "<-"), (Code, "also some code")]
          expected = (before, middle, after)
      splitSource [] sndSplit sample `shouldBe` expected

    it "splits source code with both conditions" $ do
      let before =
            SourceCode [(Code, "some code"), (Comment, "->"), (Code, "->")]
          middle   = SourceCode [(Code, "RESULT"), (Comment, "<-")]
          after    = SourceCode [(Code, "<-"), (Code, "also some code")]
          expected = (before, middle, after)
      splitSource fstSplit sndSplit sample `shouldBe` expected

    it "splits source code when nothing matches the 1st split condition" $ do
      let sample' = SourceCode
            [ (Code   , "some code")
            , (Comment, "->")
            , (Code   , "RESULT")
            , (Comment, "<-")
            , (Code   , "<-")
            , (Code   , "also some code")
            ]
          expected = (mempty, sample', mempty)
      splitSource fstSplit [] sample' `shouldBe` expected

    it "splits source code when nothing matches the 2nd split condition" $ do
      let sample' = SourceCode
            [ (Code   , "some code")
            , (Comment, "->")
            , (Code   , "->")
            , (Code   , "RESULT")
            , (Comment, "<-")
            , (Code   , "also some code")
            ]
          expected = (mempty, sample', mempty)
      splitSource [] sndSplit sample' `shouldBe` expected

    it "splits source code when nothing matches both conditions" $ do
      let sample' = SourceCode
            [ (Code   , "some code")
            , (Comment, "->")
            , (Code   , "RESULT")
            , (Comment, "<-")
            , (Code   , "also some code")
            ]
          expected = (mempty, sample', mempty)
      splitSource fstSplit sndSplit sample' `shouldBe` expected

    it "handles case when 2nd split is found before 1st split" $ do
      let before = mempty
          middle = SourceCode [(Code, "some code"), (Comment, "->")]
          after  = SourceCode
            [ (Code   , "->")
            , (Code   , "RESULT")
            , (Comment, "<-")
            , (Code   , "<-")
            , (Code   , "also some code")
            ]
          expected = (before, middle, after)
      splitSource sndSplit fstSplit sample `shouldBe` expected

    it "handles case when 1st split is also after 2nd split" $ do
      let
        sample' = SourceCode
          [ (Code   , "some code")
          , (Comment, "->")
          , (Code   , "->")
          , (Code   , "RESULT")
          , (Comment, "<-")
          , (Code   , "<-")
          , (Code   , "->")
          , (Code   , "also some code")
          ]
        before =
          SourceCode [(Code, "some code"), (Comment, "->"), (Code, "->")]
        middle = SourceCode [(Code, "RESULT"), (Comment, "<-")]
        after =
          SourceCode [(Code, "<-"), (Code, "->"), (Code, "also some code")]
        expected = (before, middle, after)
      splitSource fstSplit sndSplit sample' `shouldBe` expected
