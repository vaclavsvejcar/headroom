{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Headroom.SourceCodeSpec
  ( spec
  )
where

import           Headroom.SourceCode
import           RIO
import qualified RIO.Text                           as T
import           Test.Hspec


spec :: Spec
spec = do

  describe "fromText" $ do
    it "converts Text to SourceCode" $ do
      let
        expected = SourceCode
          [ (Comment, "-- some comment")
          , (Code   , "some code")
          , (Code   , "another code")
          ]
        sample = "-- some comment\nsome code\nanother code"
        f = \line -> pure $ if "--" `T.isPrefixOf` line then Comment else Code
      fromText [] f sample `shouldBe` expected


  describe "toText" $ do
    it "converts SourceCode to Text" $ do
      let expected = "-- some comment\nsome code\nanother code"
          sample   = SourceCode
            [ (Comment, "-- some comment")
            , (Code   , "some code")
            , (Code   , "another code")
            ]
      toText sample `shouldBe` expected


  describe "firstMatching" $ do
    it "finds very first line matching the given predicate" $ do
      let sample = SourceCode
            [ (Comment, "/*")
            , (Comment, "this is block comment")
            , (Comment, "/* this is nested comment */")
            , (Comment, "// also nested comment")
            , (Comment, "/*")
            , (Comment, "this is also nested comment")
            , (Comment, "*/")
            , (Comment, "this is still comment")
            , (Comment, "*/")
            , (Code   , "this is some code")
            , (Code   , "this is code with // comment")
            , (Comment, "// single line comment")
            ]
          expected  = Just (9, (Code, "this is some code"))
          predicate = \(lt, l) -> lt == Code && "this" `T.isPrefixOf` l
      firstMatching predicate sample `shouldBe` expected


  describe "lastMatching" $ do
    it "finds very last line matching the given predicate" $ do
      let sample = SourceCode
            [ (Comment, "/*")
            , (Comment, "this is block comment")
            , (Comment, "/* this is nested comment */")
            , (Comment, "// also nested comment")
            , (Comment, "/*")
            , (Comment, "this is also nested comment")
            , (Comment, "*/")
            , (Comment, "this is still comment")
            , (Comment, "*/")
            , (Code   , "this is some code")
            , (Code   , "this is code with // comment")
            , (Comment, "// single line comment")
            ]
          expected  = Just (10, (Code, "this is code with // comment"))
          predicate = \(lt, l) -> lt == Code && "this" `T.isPrefixOf` l
      lastMatching predicate sample `shouldBe` expected
