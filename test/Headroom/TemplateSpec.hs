{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.TemplateSpec
  ( spec
  )
where

import           Headroom.Template
import           Test.Hspec
import           RIO
import qualified RIO.Text                      as T

spec :: Spec
spec = do
  describe "parseTemplate" $ do
    it "parses valid Ginger template from raw string" $ do
      let raw    = "Hello, {{name}}"
      let parsed = parseTemplate (T.pack raw)
      parsed `shouldSatisfy` isRight

    it "parses valid Ginger template from template file" $ do
      parsed <- parseTemplateFile "test-data/test-template.jinja2"
      parsed `shouldSatisfy` isRight
