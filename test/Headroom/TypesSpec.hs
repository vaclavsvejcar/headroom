{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.TypesSpec
  ( spec
  )
where

import           Headroom.Types
import           Test.Hspec
import           RIO
import qualified RIO.HashMap                   as HM


spec :: Spec
spec = do
  let ph1        = HM.fromList [("key1", "value1")]
      ph2        = HM.fromList [("key2", "value2")]
      appConfig1 = AppConfig 1 False ["source1"] [] ph1
      appConfig2 = AppConfig 2 True ["source2"] ["template1"] ph2
      expected   = AppConfig
        1
        False
        ["source1", "source2"]
        ["template1"]
        (HM.fromList [("key1", "value1"), ("key2", "value2")])

  describe "<>" $ do
    it "joins two AppConfig records" $ do
      (appConfig1 <> appConfig2) `shouldBe` expected

  describe "mconcat" $ do
    it "folds a list of AppConfig records" $ do
      mconcat [appConfig1, appConfig2] `shouldBe` expected

