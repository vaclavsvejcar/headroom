{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.VariablesSpec
  ( spec
  )
where

import           Headroom.Types                 ( Variables(..) )
import           Headroom.Variables             ( mkVariables
                                                , parseVariables
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import           Test.Hspec


spec :: Spec
spec = do
  describe "mkVariables" $ do
    it "constructs new Variables" $ do
      let sample   = [("key1", "value1")]
          expected = Variables $ HM.fromList sample
      mkVariables sample `shouldBe` expected


  describe "parseVariables" $ do
    it "parses variables from key=value textual representation" $ do
      let sample   = ["key1=value1", "key2=value2"]
          expected = mkVariables [("key1", "value1"), ("key2", "value2")]
      parseVariables sample `shouldBe` Just expected
