{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Headroom.Command.Readers
Description : Custom readers for /optparse-applicative/ library
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains custom readers required by the /optparse-applicative/
library to parse data types such as 'LicenseType' or 'FileType'.
-}

module Headroom.Command.Readers
  ( licenseReader
  , licenseTypeReader
  , parseLicenseAndFileType
  )
where

import           Data.Either.Combinators        ( maybeToRight )
import           Headroom.Data.EnumExtra        ( EnumExtra(..) )
import           Headroom.Types                 ( FileType
                                                , LicenseType
                                                )
import           Options.Applicative
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP



-- | Reader for tuple of 'LicenseType' and 'FileType'.
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


-- | Reader for 'LicenseType'.
licenseTypeReader :: ReadM LicenseType
licenseTypeReader = eitherReader parseLicenseType
 where
  parseLicenseType raw = maybeToRight errMsg (textToEnum $ T.pack raw)
  errMsg = T.unpack $ mconcat
    [ "invalid license type, available options: "
    , T.toLower (allValuesToText @LicenseType)
    ]


-- | Parses 'LicenseType' and 'FileType' from the input string,
-- formatted as @licenseType:fileType@.
--
-- >>> parseLicenseAndFileType "bsd3:haskell"
-- Just (BSD3,Haskell)
parseLicenseAndFileType :: Text -> Maybe (LicenseType, FileType)
parseLicenseAndFileType raw
  | [rawLicenseType, rawFileType] <- TP.splitOn ":" raw = do
    licenseType <- textToEnum rawLicenseType
    fileType    <- textToEnum rawFileType
    pure (licenseType, fileType)
  | otherwise = Nothing
