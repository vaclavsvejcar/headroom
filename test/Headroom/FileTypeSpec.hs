{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.FileTypeSpec (
    spec
) where

import Headroom.Config (
    makeHeadersConfig
    , parseAppConfig
 )
import Headroom.Config.Types (AppConfig (..))
import Headroom.Embedded (defaultConfig)
import Headroom.FileType
import Headroom.FileType.Types (FileType (..))
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "fileTypeByExt" $ do
        it "parses FileType from file extension" $ do
            pHeadersConfig <- acLicenseHeaders <$> parseAppConfig defaultConfig
            headersConfig <- makeHeadersConfig pHeadersConfig
            fileTypeByExt headersConfig "hs" `shouldBe` Just Haskell
