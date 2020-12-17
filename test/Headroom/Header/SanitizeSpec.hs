{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Headroom.Header.SanitizeSpec
  ( spec
  )
where

import           Headroom.Configuration.Types        ( HeaderSyntax(..) )
import           Headroom.Data.TextExtra             ( fromLines )
import           Headroom.Header.Sanitize
import           RIO
import           Test.Hspec



spec :: Spec
spec = do

  describe "findPrefix" $ do
    it "finds and fills line prefix to LineComment header syntax" $ do
      let sample   = fromLines ["-- first", "", "-- second", "-- third"]
          syntax   = LineComment "--" Nothing
          expected = LineComment "--" (Just "--")
      findPrefix syntax sample `shouldBe` expected

    it "finds and fills line prefix to BlockComment header syntax" $ do
      let sample   = fromLines ["{-", " - foo", " - bar", " -}"]
          syntax   = BlockComment "{-" "-}" Nothing
          expected = BlockComment "{-" "-}" (Just " -")
      findPrefix syntax sample `shouldBe` expected
