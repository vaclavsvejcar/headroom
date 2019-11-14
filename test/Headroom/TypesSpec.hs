{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.TypesSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Types
import           RIO

spec :: Spec
spec = do
  describe "dropFieldPrefix" $ do
    it "removes prefix and lowercases first letter for 'prSomeField'" $ do
      dropFieldPrefix "prSomeField" `shouldBe` "someField"

    it "removes prefix and keeps case for 'prURLField'" $ do
      dropFieldPrefix "prURLField" `shouldBe` "URLField"
