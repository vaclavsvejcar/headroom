{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Command.ReadersSpec (
    spec
) where

import Headroom.Command.Readers
import Headroom.Config.Types (LicenseType)
import Headroom.Data.EnumExtra (EnumExtra (..))
import Headroom.FileType.Types (FileType)
import RIO
import qualified RIO.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
    describe "parseLicense" $ do
        prop "should parse license and file type from raw input" prop_parseLicense
  where
    licenseTypes = T.toLower . enumToText <$> allValues @LicenseType
    fileTypes = T.toLower . enumToText <$> allValues @FileType
    together = \lt -> fmap (\ft -> lt <> ":" <> ft) fileTypes
    licenseAndFileTypesGen = elements $ concatMap together licenseTypes
    prop_parseLicense = forAll licenseAndFileTypesGen (isJust . parseLicense)
