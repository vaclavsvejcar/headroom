{-|
Module      : Headroom.Types
Description : Data types and instances
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Shared data types and type class instances.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( HeadroomError(..)
  , NewLine(..)
  , Progress(..)
  )
where

import           RIO
import qualified RIO.List                      as L
import           RIO.Text                       ( Text )
import qualified RIO.Text                      as T
import           Text.Printf                    ( printf )


data HeadroomError
  = InvalidLicense Text
  | InvalidPlaceholder Text
  | NoGenModeSelected
  | MissingPlaceholders Text [Text]
  | ParseError Text
  deriving (Show, Typeable)

data NewLine = CR | CRLF | LF deriving (Eq, Show)

data Progress = Progress Int Int
  deriving Eq

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
