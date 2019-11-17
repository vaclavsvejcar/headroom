{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.TypesSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Types
import           RIO
import           RIO.List                       ( sort )

spec :: Spec
spec = do
  describe "allValues" $ do
    it "should list all values of FileType enum" $ do
      let actual   = allValues :: [FileType]
          expected = [CSS, Haskell, HTML]
      sort actual `shouldBe` sort expected

  describe "dropFieldPrefix" $ do
    it "removes prefix and lowercases first letter for 'prSomeField'" $ do
      dropFieldPrefix "prSomeField" `shouldBe` "someField"

    it "removes prefix and keeps case for 'prURLField'" $ do
      dropFieldPrefix "prURLField" `shouldBe` "URLField"
