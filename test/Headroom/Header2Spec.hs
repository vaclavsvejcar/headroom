{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Headroom.Header2Spec
  ( spec
  )
where

import           Headroom.Data.Regex                 ( re )
import           Headroom.Header2
import           Headroom.SourceCode                 ( LineType(..)
                                                     , SourceCode(..)
                                                     )
import           RIO
import           Test.Hspec                   hiding ( after
                                                     , before
                                                     )


spec :: Spec
spec = do

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
