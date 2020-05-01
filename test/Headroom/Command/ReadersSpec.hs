{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Headroom.Command.ReadersSpec
  ( spec
  )
where

import           Headroom.Command.Readers
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Types                 ( FileType
                                                , LicenseType
                                                )
import           RIO
import qualified RIO.Text                      as T
import           Test.Hspec
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck


spec :: Spec
spec = do
  describe "parseLicenseAndFileType" $ do
    prop "should parse license and file type from raw input"
         prop_parseLicenseAndFileType

 where
  licenseTypes = fmap (T.toLower . enumToText) (allValues @LicenseType)
  fileTypes = fmap (T.toLower . enumToText) (allValues @FileType)
  licenseAndFileTypesGen = elements
    $ concatMap (\lt -> fmap (\ft -> lt <> ":" <> ft) fileTypes) licenseTypes
  prop_parseLicenseAndFileType =
    forAll licenseAndFileTypesGen (isJust . parseLicenseAndFileType)
