{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.SourceCodeSpec (
    spec
) where

import Headroom.SourceCode
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "fromText" $ do
        it "converts Text to SourceCode" $ do
            let expected =
                    SourceCode
                        [ (Comment, "-- some comment")
                        , (Code, "some code")
                        , (Code, "another code")
                        ]
                sample = "-- some comment\nsome code\nanother code"
                f = \line -> pure $ if "--" `T.isPrefixOf` line then Comment else Code
            fromText [] f sample `shouldBe` expected

    describe "toText" $ do
        it "converts SourceCode to Text" $ do
            let expected = "-- some comment\nsome code\nanother code"
                sample =
                    SourceCode
                        [ (Comment, "-- some comment")
                        , (Code, "some code")
                        , (Code, "another code")
                        ]
            toText sample `shouldBe` expected

    describe "firstMatching" $ do
        it "finds and transforms very first line matching the given predicate" $ do
            let sample =
                    SourceCode
                        [ (Comment, "/*")
                        , (Comment, "this is block comment")
                        , (Comment, "/* this is nested comment */")
                        , (Comment, "// also nested comment")
                        , (Comment, "/*")
                        , (Comment, "this is also nested comment")
                        , (Comment, "*/")
                        , (Comment, "this is still comment")
                        , (Comment, "*/")
                        , (Code, "this is some code")
                        , (Code, "this is code with // comment")
                        , (Comment, "// single line comment")
                        ]
                expected = Just (9, "THIS IS SOME CODE")
                f = \(lt, l) ->
                    if lt == Code && "this" `T.isPrefixOf` l
                        then Just $ T.toUpper l
                        else Nothing
            firstMatching f sample `shouldBe` expected

    describe "lastMatching" $ do
        it "finds and transforms very last line matching the given predicate" $ do
            let sample =
                    SourceCode
                        [ (Comment, "/*")
                        , (Comment, "this is block comment")
                        , (Comment, "/* this is nested comment */")
                        , (Comment, "// also nested comment")
                        , (Comment, "/*")
                        , (Comment, "this is also nested comment")
                        , (Comment, "*/")
                        , (Comment, "this is still comment")
                        , (Comment, "*/")
                        , (Code, "this is some code")
                        , (Code, "this is code with // comment")
                        , (Comment, "// single line comment")
                        ]
                expected = Just (10, "THIS IS CODE WITH // COMMENT")
                f = \(lt, l) ->
                    if lt == Code && "this" `T.isPrefixOf` l
                        then Just $ T.toUpper l
                        else Nothing
            lastMatching f sample `shouldBe` expected

    describe "stripStart" $ do
        it "strips empty lines from the start of the source code" $ do
            let sample = SourceCode [(Code, ""), (Code, ""), (Code, "h"), (Code, "")]
                expected = SourceCode [(Code, "h"), (Code, "")]
            stripStart sample `shouldBe` expected

    describe "stripEnd" $ do
        it "strips empty lines from the start of the source code" $ do
            let sample = SourceCode [(Code, ""), (Code, "h"), (Code, ""), (Code, "")]
                expected = SourceCode [(Code, ""), (Code, "h")]
            stripEnd sample `shouldBe` expected

    describe "cut" $ do
        it "cuts source code using the given start and end positions" $ do
            let sample =
                    SourceCode [(Code, "1"), (Code, "2"), (Code, "3"), (Code, "4")]
                expected = SourceCode [(Code, "2"), (Code, "3")]
            cut 1 3 sample `shouldBe` expected
