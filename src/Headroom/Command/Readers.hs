{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Command.Readers
  ( licenseReader
  , licenseTypeReader
  , parseLicenseAndFileType
  )
where

import           Data.Either.Combinators        ( maybeToRight )
import           Headroom.Types                 ( FileType
                                                , LicenseType
                                                )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
import           Options.Applicative
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP


licenseReader :: ReadM (LicenseType, FileType)
licenseReader = eitherReader parseLicense
 where
  parseLicense raw = maybeToRight errMsg (parseLicenseAndFileType $ T.pack raw)
  errMsg = T.unpack $ mconcat
    [ "invalid license/file type, must be in format 'licenseType:fileType' "
    , "(e.g. bsd3:haskell)"
    , "\nAvailable license types: "
    , T.toLower (allValuesToText @LicenseType)
    , "\nAvailable file types: "
    , T.toLower (allValuesToText @FileType)
    ]

licenseTypeReader :: ReadM LicenseType
licenseTypeReader = eitherReader parseLicenseType
 where
  parseLicenseType raw = maybeToRight errMsg (textToEnum $ T.pack raw)
  errMsg = T.unpack $ mconcat
    [ "invalid license type, available options: "
    , T.toLower (allValuesToText @LicenseType)
    ]


parseLicenseAndFileType :: Text -> Maybe (LicenseType, FileType)
parseLicenseAndFileType raw
  | [rawLicenseType, rawFileType] <- TP.splitOn ":" raw = do
    licenseType <- textToEnum rawLicenseType
    fileType    <- textToEnum rawFileType
    pure (licenseType, fileType)
  | otherwise = Nothing
