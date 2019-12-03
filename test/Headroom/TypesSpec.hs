{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.TypesSpec
  ( spec
  )
where

import           Headroom.Types
import           Test.Hspec
import           RIO


spec :: Spec
spec = do
  describe "show" $ do
    it "shows correct output for Progress data type" $ do
      show (Progress 1 1) `shouldBe` "[1 of 1]"
      show (Progress 10 250) `shouldBe` "[ 10 of 250]"


