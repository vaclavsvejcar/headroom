{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}

{-|
Module      : Headroom.Types
Description : Application data types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module containing most of the data types used by the application.
-}

module Headroom.Types
  ( -- * Error Data Types
    HeadroomError(..)
    -- ** Helper Functions
  , fromHeadroomError
  , toHeadroomError
    -- * Other Data Types
  , TemplateMeta(..)
  , HaddockFieldOffsets(..)
  , CurrentYear(..)
  )
where

import           Data.Default.Class             ( Default(..) )
import           Data.Typeable                  ( cast )
import           RIO


-- | Top-level of the /Headroom/ exception hierarchy.
data HeadroomError = forall e . Exception e => HeadroomError e

instance Show HeadroomError where
  show (HeadroomError he) = show he

instance Exception HeadroomError where
  displayException (HeadroomError he) = displayException he


-- | Wraps given exception into 'HeadroomError'.
toHeadroomError :: Exception e
                => e
                -- ^ exception to wrap
                -> SomeException
                -- ^ wrapped exception
toHeadroomError = toException . HeadroomError


-- | Unwraps given exception from 'HeadroomError'.
fromHeadroomError :: Exception e
                  => SomeException
                  -- ^ exception to unwrap
                  -> Maybe e
                  -- ^ unwrapped exception
fromHeadroomError e = do
  HeadroomError he <- fromException e
  cast he


-- | Wraps the value of current year.
newtype CurrentYear = CurrentYear
  { unCurrentYear :: Integer
  -- ^ value of current year
  }
  deriving (Eq, Show)


-- | Offsets for selected fields extracted from /Haddock module header/.
data HaddockFieldOffsets = HaddockFieldOffsets
  { hfoCopyright :: Maybe Int
  -- ^ offset for /Copyright/ field
  }
  deriving (Eq, Show)

instance Default HaddockFieldOffsets where
  def = HaddockFieldOffsets { hfoCopyright = Nothing }

-- | Metadata parsed from raw /template/, specific for selected /file type/.
data TemplateMeta = HaskellTemplateMeta HaddockFieldOffsets
  deriving (Eq, Show)

