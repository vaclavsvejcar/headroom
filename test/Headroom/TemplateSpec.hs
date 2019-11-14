{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.TemplateSpec
  ( spec
  )
where

import           Headroom.Template
import           Test.Hspec
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

spec :: Spec
spec = do
  describe "parseTemplate" $ do
    it "parses valid Ginger template from raw string" $ do
      let raw    = "Hello, {{name}}"
          parsed = parseTemplate (T.pack raw)
      parsed `shouldSatisfy` isRight

  describe "parseTemplateFile" $ do
    it "parses valid Ginger template from template file" $ do
      parsed <- parseTemplateFile "test-data/test-template.jinja2"
      parsed `shouldSatisfy` isRight

  describe "renderTemplate" $ do
    it "renders template from raw string with given context" $ do
      let raw      = "Hello, {{name}}"
          parsed   = either (error . show) id (parseTemplate (T.pack raw))
          context' = HM.fromList [("name", "John")]
          rendered = renderTemplate context' parsed
      rendered `shouldBe` "Hello, John"
