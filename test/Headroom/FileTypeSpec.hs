{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileTypeSpec
  ( spec
  )
where

import           Headroom.Configuration         ( makeHeadersConfig
                                                , parseConfiguration
                                                )
import           Headroom.Configuration.Types   ( Configuration(..) )
import           Headroom.Embedded              ( defaultConfig )
import           Headroom.FileType
import           Headroom.FileType.Types        ( FileType(..) )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "fileTypeByExt" $ do
    it "parses FileType from file extension" $ do
      pHeadersConfig <- cLicenseHeaders <$> parseConfiguration defaultConfig
      headersConfig  <- makeHeadersConfig pHeadersConfig
      fileTypeByExt headersConfig "hs" `shouldBe` Just Haskell
