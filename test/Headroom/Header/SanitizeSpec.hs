{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Headroom.Header.SanitizeSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( HeaderSyntax(..) )
import           Headroom.Data.Regex                 ( re )
import           Headroom.Data.Text                  ( fromLines )
import           Headroom.Header.Sanitize
import           RIO
import           Test.Hspec


spec :: Spec
spec = do

  describe "findPrefix" $ do
    it "finds and fills line prefix to LineComment header syntax" $ do
      let s        = [re|^--|]
          sample   = fromLines ["-- first", "", "-- second", "-- third"]
          syntax   = LineComment s Nothing
          expected = LineComment s (Just "--")
      findPrefix syntax sample `shouldBe` expected

    it "finds and fills line prefix to BlockComment header syntax" $ do
      let s        = [re|^{-\||]
          e        = [re|(?<!#)-}$|]
          sample   = fromLines ["{-|", " - foo", " - bar", " -}"]
          syntax   = BlockComment s e Nothing
          expected = BlockComment s e (Just " -")
      findPrefix syntax sample `shouldBe` expected


  describe "sanitizeSyntax" $ do
    it "sanitizes syntax for line comment with prefix" $ do
      let syntax   = LineComment [re|^--|] (Just "--")
          sample   = fromLines ["-- first", "second", "-- third"]
          expected = fromLines ["-- first", "-- second", "-- third"]
      sanitizeSyntax syntax sample `shouldBe` expected

    it "sanitizes syntax for block comment with prefix" $ do
      let syntax   = BlockComment [re|^\/\*|] [re|\*\/$|] (Just " *")
          sample   = fromLines ["/*", " * first", "second", " */"]
          expected = fromLines ["/*", " * first", " * second", " */"]
      sanitizeSyntax syntax sample `shouldBe` expected

    it "does nothing for already valid header" $ do
      let syntax = LineComment [re|^--|] (Just "--")
          sample = fromLines ["-- first", "-- second", "-- third"]
      sanitizeSyntax syntax sample `shouldBe` sample

    it "does nothing when prefix is unknown" $ do
      let syntax = LineComment [re|^--|] Nothing
          sample = fromLines ["-- first", "second", "-- third"]
      sanitizeSyntax syntax sample `shouldBe` sample


  describe "stripCommentSyntax" $ do
    it "strips comment syntax from single line block comment (no prefix)" $ do
      let syntax   = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
          sample   = "{-| outer {- and inner -} comment -}"
          expected = "outer {- and inner -} comment"
      stripCommentSyntax syntax sample `shouldBe` expected

    it "strips comment syntax from single line line comment (no prefix)" $ do
      let syntax   = LineComment [re|^--|] Nothing
          sample   = "-- single line comment"
          expected = "single line comment"
      stripCommentSyntax syntax sample `shouldBe` expected

    it "strips comment syntax from multi line block comment (no prefix)" $ do
      let syntax = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
          sample = fromLines
            [ "{-|"
            , "Some block comment"
            , "another line"
            , "@"
            , "  {- code example comment -}"
            , "@"
            , "-}"
            ]
          expected = fromLines
            [ "Some block comment"
            , "another line"
            , "@"
            , "  {- code example comment -}"
            , "@"
            ]
      stripCommentSyntax syntax sample `shouldBe` expected

    it "strips comment syntax from multi line block comment (with prefix)" $ do
      let
        syntax = BlockComment [re|^\/\*|] [re|\*\/$|] (Just " * ")
        sample = fromLines
          [ "/*"
          , " * Some block comment"
          , " * another line"
          , " * @"
          , " *  /* code example comment */"
          , " */"
          ]
        expected = fromLines
          [ "Some block comment"
          , "another line"
          , "@"
          , " /* code example comment */"
          ]
      stripCommentSyntax syntax sample `shouldBe` expected

    it "strips comment syntax from multi line line comment" $ do
      let
        syntax = LineComment [re|^--|] (Just "-- ")
        sample = fromLines
          [ "-- Some block comment"
          , "-- another line"
          , "-- @"
          , "--  /* code example comment */"
          ]
        expected = fromLines
          [ "Some block comment"
          , "another line"
          , "@"
          , " /* code example comment */"
          ]
      stripCommentSyntax syntax sample `shouldBe` expected
