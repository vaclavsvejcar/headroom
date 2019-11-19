{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types.UtilSpec
  ( spec
  )
where

import           Test.Hspec
import           Headroom.Types
import           Headroom.Types.Util
import           RIO
import           RIO.List                       ( sort )


spec :: Spec
spec = do
  describe "allValues" $ do
    it "should list all values of FileType enum" $ do
      let actual   = allValues :: [FileType]
          expected = [Haskell]
      sort actual `shouldBe` sort expected

  describe "dropFieldPrefix" $ do
    it "removes prefix and lowercases first letter for 'prSomeField'" $ do
      dropFieldPrefix "prSomeField" `shouldBe` "someField"

    it "removes prefix and keeps case for 'prURLField'" $ do
      dropFieldPrefix "prURLField" `shouldBe` "URLField"

  describe "readEnumCI" $ do
    it "reads enum value from string representation (case insensitive)" $ do
      let expected = [(Haskell, "")]
      readEnumCI "haskell" `shouldBe` expected
      readEnumCI "Haskell" `shouldBe` expected
      readEnumCI "HASKELL" `shouldBe` expected

