{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Headroom.Template.MustacheSpec
  ( spec
  )
where

import           Headroom.Template
import           Headroom.Template.Mustache
import           Headroom.Types
import           RIO
import qualified RIO.HashMap                   as HM
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseTemplate" $ do
    it "parses Mustache template from raw text" $ do
      let template = "Hello, {{ name }}"
          parsed   = parseTemplate (Just "template") template :: Maybe Mustache
      parsed `shouldSatisfy` isJust

  describe "renderTemplate" $ do
    it "renders Mustache template with given variables" $ do
      let template  = "Hello, {{ name }}"
          variables = HM.fromList [("name", "John")]
          parsed    = parseTemplate (Just "template") template :: Maybe Mustache
          rendered  = parsed >>= renderTemplate variables :: Maybe Text
      rendered `shouldBe` Just "Hello, John"

    it "fails if not enough variables is provided" $ do
      let
        template  = "Hello, {{ name }} {{ surname }}"
        variables = HM.fromList [("name", "John")]
        parsed :: Either SomeException Mustache
        parsed   = parseTemplate (Just "test") template
        rendered = parsed >>= renderTemplate variables
        handleResult (Left ex)
          | Just (MissingVariables "test" ["surname"]) <- fromException ex
          = True
          | otherwise
          = False
        handleResult _ = False
      rendered `shouldSatisfy` handleResult
