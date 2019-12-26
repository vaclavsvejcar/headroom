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
    it "renders Mustache template with given placeholders" $ do
      let template     = "Hello, {{ name }}"
          placeholders = HM.fromList [("name", "John")]
          parsed = parseTemplate (Just "template") template :: Maybe Mustache
          rendered     = parsed >>= renderTemplate placeholders :: Maybe Text
      rendered `shouldBe` Just "Hello, John"

    it "fails if not enough placeholders is provided" $ do
      let
        template     = "Hello, {{ name }} {{ surname }}"
        placeholders = HM.fromList [("name", "John")]
        parsed :: Either SomeException Mustache
        parsed   = parseTemplate (Just "test") template
        rendered = parsed >>= renderTemplate placeholders
        handleResult (Left ex)
          | Just (MissingPlaceholders "test" ["surname"]) <- fromException ex
          = True
          | otherwise
          = False
        handleResult _ = False
      rendered `shouldSatisfy` handleResult
