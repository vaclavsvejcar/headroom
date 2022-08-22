{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.FileSupport.CSSSpec
    ( spec
    )
where

import Headroom.FileSupport.CSS
import Headroom.FileSupport.Types
    ( FileSupport (..)
    , SyntaxAnalysis (..)
    )
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "fsSyntaxAnalysis" $ do
        it "correctly detects comment starts/ends" $ do
            let samples =
                    [ ("non comment line", (False, False))
                    , ("/* block comment start", (True, False))
                    , ("block comment end */", (False, True))
                    , ("/* block comment start/end */", (True, True))
                    ]
            all checkSyntaxAnalysis samples `shouldBe` True
  where
    checkSyntaxAnalysis (l, (s, e)) =
        let SyntaxAnalysis{..} = fsSyntaxAnalysis fileSupport
         in saIsCommentStart l == s && saIsCommentEnd l == e
