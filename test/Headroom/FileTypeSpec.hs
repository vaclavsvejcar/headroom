{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileTypeSpec
  ( spec
  )
where

import           Headroom.Configuration         ( makeHeadersConfig
                                                , parseConfiguration
                                                )
import           Headroom.Embedded              ( defaultConfig )
import           Headroom.FileType
import           Headroom.Types                 ( FileType(..)
                                                , PartialConfiguration(..)
                                                )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "fileTypeByExt" $ do
    it "parses FileType from file extension" $ do
      pHeadersConfig <- pcLicenseHeaders <$> parseConfiguration defaultConfig
      headersConfig  <- makeHeadersConfig pHeadersConfig
      fileTypeByExt headersConfig "hs" `shouldBe` Just Haskell
