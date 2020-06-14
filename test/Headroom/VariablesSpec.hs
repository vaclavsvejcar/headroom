{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.VariablesSpec
  ( spec
  )
where

import           Headroom.Types                 ( CurrentYear(..) )
import           Headroom.Variables
import           Headroom.Variables.Types       ( Variables(..) )
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


  describe "compileVariables" $ do
    it "compiles template-like variable values" $ do
      let sample1 = mkVariables
            [("name", "John Smith"), ("greeting", "Hello, {{ name }}")]
          expected = mkVariables
            [("name", "John Smith"), ("greeting", "Hello, John Smith")]
      result <- compileVariables sample1
      result `shouldBe` expected

    it "doesn't get stuck in infinite loop on invalid recursive variable" $ do
      let sample1  = mkVariables [("greeting", "Hello, {{ greeting }}")]
          expected = mkVariables [("greeting", "Hello, Hello, {{ greeting }}")]
      result <- compileVariables sample1
      result `shouldBe` expected


  describe "dynamicVariables" $ do
    it "returns map of all expected dynamic variables" $ do
      let year     = CurrentYear 2020
          expected = mkVariables [("_current_year", "2020")]
      dynamicVariables year `shouldBe` expected
