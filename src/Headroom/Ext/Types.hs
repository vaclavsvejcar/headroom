{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Ext.Types
Description : Data types for extended support functionality
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types for "Headroom.Ext" module.
-}

module Headroom.Ext.Types
  ( ExtData(..)
  , HaddockOffsets(..)
  , HaskellExtData'(..)
  )
where

import           RIO


---------------------------------  DATA TYPES  ---------------------------------

-- | Additional data that might be required by concrete /extended support/.
data ExtData
  = HaskellExtData HaskellExtData'
  -- ^ additional data for /Haskell/ extended support
  | NoExtData
  -- ^ no additional data for extended support available


-- | Offsets for selected fields extracted from /Haddock module header/.
data HaddockOffsets = HaddockOffsets
  { hoCopyright :: Maybe Int
  -- ^ offset for /Copyright/ field
  }
  deriving (Eq, Show)


-- | Additional data required by /Haskell/ extended support
data HaskellExtData' = HaskellExtData'
  { hedHaddockOffsets :: HaddockOffsets
  -- ^ offsets for /Haddock/ fields
  }
  deriving (Eq, Show)
