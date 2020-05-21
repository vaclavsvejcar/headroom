{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Headroom.Template.MustacheSpec
  ( spec
  )
where

import           Headroom.Template
import           Headroom.Template.Mustache
import           Headroom.Types                 ( ApplicationError(..)
                                                , TemplateError(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import           Test.Hspec
import           Test.Utils                     ( matchesException )

spec :: Spec
spec = do
  describe "parseTemplate" $ do
    it "parses Mustache template from raw text" $ do
      let template = "Hello, {{ name }}"
          parsed   = parseTemplate @Mustache (Just "template") template
      parsed `shouldSatisfy` isJust

  describe "renderTemplate" $ do
    it "renders template with given variables" $ do
      let template  = "Hello, {{ name }}"
          variables = HM.fromList [("name", "John")]
          parsed    = parseTemplate @Mustache (Just "template") template
          rendered  = parsed >>= renderTemplate variables
      rendered `shouldBe` Just "Hello, John"

    it "fails if not enough variables is provided" $ do
      let
        template  = "Hello, {{ name }} {{ surname }}"
        variables = HM.fromList [("name", "John")]
        parsed    = parseTemplate @Mustache (Just "test") template
        rendered  = parsed >>= renderTemplate variables
        check (Just (TemplateError (MissingVariables "test" ["surname"]))) =
          True
        check _ = False
      rendered `shouldSatisfy` matchesException check

    it "renders template with conditionally set variable" $ do
      let template  = "Foo {{#bar}}{{bar}}{{/bar}}{{^bar}}BAR{{/bar}}"
          variables = HM.empty
          parsed    = parseTemplate @Mustache (Just "template") template
          rendered  = parsed >>= renderTemplate variables
      rendered `shouldBe` Just "Foo BAR"

    it "fails if non-existing variable is used with inverted sections" $ do
      let template  = "Foo {{bar}}{{^bar}}BAR{{/bar}}"
          variables = HM.fromList [("xx", "yy")]
          parsed    = parseTemplate @Mustache (Just "template") template
          rendered  = parsed >>= renderTemplate variables
      rendered `shouldBe` Nothing
