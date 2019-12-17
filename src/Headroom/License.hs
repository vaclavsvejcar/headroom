{-|
Module      : Headroom.License
Description : Supported license types
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and functions for representing supported license types, that can be
generated using the @headroom gen@ command.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.License
  ( License(..)
  , LicenseType(..)
  , parseLicense
  )
where

import           Headroom.FileType              ( FileType(..)
                                                , fileTypeByName
                                                )
import           Headroom.Types.Utils           ( readEnumCI )
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP
import           Text.Read                      ( readsPrec )


-- | Represents type of the license.
data LicenseType = BSD3 -- ^ /BSD-3/ license
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Represents license (specified by 'LicenseType' and 'FileType')
data License = License LicenseType FileType
  deriving (Show, Eq)

instance Read LicenseType where
  readsPrec _ = readEnumCI

-- | Parses 'License' from the raw string representation, formatted as
-- @licenseType:fileType@.
--
-- >>> parseLicense "bsd3:haskell"
-- Just (License BSD3 Haskell)
parseLicense :: T.Text        -- ^ raw string representation
             -> Maybe License -- ^ parsed 'License'
parseLicense raw = case TP.splitOn ":" raw of
  [rawLicenseType, rawFileType] -> do
    licenseType <- parseLicenseType rawLicenseType
    fileType    <- fileTypeByName rawFileType
    return $ License licenseType fileType
  _ -> Nothing
  where parseLicenseType = readMaybe . T.unpack
