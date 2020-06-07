{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.RegexSpec
  ( spec
  )
where

import           Headroom.Regex
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "match" $ do
    it "matches regular expression against given sample" $ do
      let regex = [re|foo|bar|]
      match regex "xxx" `shouldSatisfy` isNothing
      match regex "foz" `shouldSatisfy` isNothing
      match regex "foosdas" `shouldSatisfy` isJust
      match regex "barfoo" `shouldSatisfy` isJust
