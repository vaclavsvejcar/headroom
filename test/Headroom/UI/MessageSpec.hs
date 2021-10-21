{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Headroom.UI.MessageSpec
  ( spec
  )
where

import           Headroom.UI.Message
import           RIO
import           Test.Hspec

spec :: Spec
spec = do

  describe "Message" $ do
    it "properly displays info message" $ do
      textDisplay (messageInfo "info message") `shouldBe` "[i] info message"

    it "properly displays warn message" $ do
      textDisplay (messageWarn "warn message") `shouldBe` "[!] warn message"

    it "properly displays error message" $ do
      textDisplay (messageError "error message") `shouldBe` "[x] error message"
