module Headroom.TemplateSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Template

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
