{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.TypesSpec
  ( spec
  )
where

import           Data.Aeson                     ( eitherDecode )
import           Headroom.Types
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "show" $ do
    it "displays correct output for Progress data type" $ do
      textDisplay (Progress 1 1) `shouldBe` "[1 of 1]"
      textDisplay (Progress 10 250) `shouldBe` "[ 10 of 250]"

  describe "parseJSON" $ do
    it "should parse RunMode value" $ do
      eitherDecode "\"add\"" `shouldBe` Right Add
      eitherDecode "\"drop\"" `shouldBe` Right Drop
      eitherDecode "\"replace\"" `shouldBe` Right Replace
      eitherDecode "\"ADD\"" `shouldBe` Right Add
      eitherDecode "\"DROP\"" `shouldBe` Right Drop
      eitherDecode "\"REPLACE\"" `shouldBe` Right Replace

