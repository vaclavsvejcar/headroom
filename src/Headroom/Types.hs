{-|
Module      : Headroom.Types
Description : Data types and instances
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and type class instances shared between modules.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Types
  ( HeadroomError(..)
  , NewLine(..)
  , Progress(..)
  , RunMode(..)
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(String)
                                                )
import           RIO
import qualified RIO.List                      as L
import           RIO.Text                       ( Text )
import qualified RIO.Text                      as T
import           Text.Printf                    ( printf )


-- | Represents fatal application error, that should be displayed to user in
-- some human readable form.
data HeadroomError
  = InvalidLicense Text             -- ^ unknown license is selected in /Generator/
  | InvalidPlaceholder Text         -- ^ invalid placeholder format (@key=value@)
  | NoGenModeSelected               -- ^ no mode for /Generator/ command is selected
  | MissingPlaceholders Text [Text] -- ^ not all placeholders were filled in template
  | ParseError Text                 -- ^ error parsing template file
  deriving (Show, Typeable)

-- | Represents newline separator.
data NewLine
  = CR   -- ^ line ends with @\r@
  | CRLF -- ^ line ends with @\r\n@
  | LF   -- ^ line ends with @\n@
  deriving (Eq, Show)

-- | Progress indication. First argument is current progress, second the maximum
-- value.
data Progress = Progress Int Int
  deriving Eq

-- | Mode of the /Run/ command, states how to license headers in source code
-- files.
data RunMode
  = Add     -- ^ add license header if missing in source code file
  | Drop    -- ^ drop any license header if present in source code file
  | Replace -- ^ replace existing or add license header
  deriving (Eq, Show)

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance Exception HeadroomError where
  displayException (InvalidLicense raw) =
    "Cannot parse license type from: " <> T.unpack raw
  displayException (InvalidPlaceholder raw) =
    "Cannot parse placeholder key=value from: " <> T.unpack raw
  displayException NoGenModeSelected
    = "Please select at least one option what to generate (see --help for details)"
  displayException (MissingPlaceholders name placeholders) =
    "Missing placeholders for template '"
      <> T.unpack name
      <> "': "
      <> show placeholders
  displayException (ParseError msg) =
    "Error parsing template: " <> T.unpack msg

instance Show Progress where
  show (Progress current total) = "[" <> currentS <> " of " <> totalS <> "]"
   where
    format   = "%" <> (show . L.length $ totalS) <> "d"
    currentS = printf format current
    totalS   = show total

instance FromJSON RunMode where
  parseJSON (String s) = case T.toLower s of
    "add"     -> return Add
    "drop"    -> return Drop
    "replace" -> return Replace
    _         -> error $ "Unknown run mode: " <> T.unpack s
  parseJSON other = error $ "Invalid value for run mode: " <> show other
