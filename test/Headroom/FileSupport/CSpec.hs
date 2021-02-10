{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Headroom.FileSupport.CSpec
  ( spec
  )
where

import           Headroom.FileSupport.C
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do

  describe "fsSyntaxAnalysis" $ do
    it "correctly detects comment starts/ends" $ do
      let samples =
            [ ("non comment line"             , (False, False))
            , ("// single line comment"       , (True, True))
            , ("not // single line comment"   , (False, False))
            , ("/* block comment start"       , (True, False))
            , ("block comment end */"         , (False, True))
            , ("/* block comment start/end */", (True, True))
            ]
      all checkSyntaxAnalysis samples `shouldBe` True

 where
  checkSyntaxAnalysis (l, (s, e)) =
    let FileSupport {..}    = fileSupport
        SyntaxAnalysis {..} = fsSyntaxAnalysis
    in  saIsCommentStart l == s && saIsCommentEnd l == e
