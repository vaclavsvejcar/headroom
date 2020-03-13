{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.LicenseSpec
  ( spec
  )
where

import           Headroom.FileType              ( FileType(..) )
import           Headroom.License
import           RIO
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseLicense" $ do
    it "parses license from raw input" $ do
      parseLicense "bsd3:haskell" `shouldBe` Just (License BSD3 Haskell)
      parseLicense "foo" `shouldBe` Nothing

  describe "parseLicenseType" $ do
    it "parses license type from raw input" $ do
      parseLicenseType "bsd3" `shouldBe` Just BSD3
      parseLicenseType "foo" `shouldBe` Nothing
