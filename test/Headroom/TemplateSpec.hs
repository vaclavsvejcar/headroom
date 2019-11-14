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

    parsed <- runIO $ parseTemplateFile "test-data/test-template.jinja2"
    it "parses valid Ginger template from template file" $ do
      parsed `shouldSatisfy` isRight
