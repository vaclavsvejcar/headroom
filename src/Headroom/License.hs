{-|
Module      : Headroom.License
Description : Representation of various license types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides data types and functions for representing various
opensource licenses, for which this application can generate /Jinja/ templates.
As the template text itself of given license may differ based on target
programming language (i.e. syntax for comments is different), each 'License' is
represented by the 'LicenseType' and 'FileType'.
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


-- | Type of the license.
data LicenseType
  = Apache2 -- ^ /Apache License, version 2.0/
  | BSD3    -- ^ /BSD-3/ license
  | GPL2    -- ^ /GNU GPL v.2/ license
  | GPL3    -- ^ /GNU GPL v.3/ license
  | MIT     -- ^ /MIT/ license
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | License (specified by 'LicenseType' and 'FileType')
data License = License LicenseType FileType
  deriving (Show, Eq)

instance Read LicenseType where
  readsPrec _ = readEnumCI

-- | Parses 'License' from the raw string representation, formatted as
-- @licenseType:fileType@.
--
-- >>> parseLicense "bsd3:haskell"
-- Just (License BSD3 Haskell)
parseLicense :: Text          -- ^ raw string representation
             -> Maybe License -- ^ parsed 'License'
parseLicense raw
  | [rawLicenseType, rawFileType] <- TP.splitOn ":" raw = do
    licenseType <- parseLicenseType rawLicenseType
    fileType    <- fileTypeByName rawFileType
    return $ License licenseType fileType
  | otherwise = Nothing
  where parseLicenseType = readMaybe . T.unpack
