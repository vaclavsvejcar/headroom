{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( HeadroomError(..)
  , NewLine(..)
  , Progress(..)
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Printf                    ( printf )


data HeadroomError
  = InvalidPlaceholder T.Text
  | NoGenModeSelected
  deriving (Show, Typeable)

data NewLine = CR | CRLF | LF deriving (Eq, Show)

data Progress = Progress Integer Integer deriving (Eq)

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance Exception HeadroomError where
  displayException (InvalidPlaceholder raw) =
    "Cannot parse placeholder key=value from: " <> T.unpack raw
  displayException NoGenModeSelected
    = "Please select at least one option what to generate (see --help for details)"

instance Show Progress where
  show (Progress current total) = "[" <> currentS <> " of " <> totalS <> "]"
   where
    format   = "%" <> (show . L.length $ totalS) <> "d"
    currentS = printf format current
    totalS   = show total
