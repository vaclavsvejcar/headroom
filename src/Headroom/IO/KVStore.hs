{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Headroom.IO.KVStore
Description : Support for key-value store implementations
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides abstract key-value store representation that allows to
implement specific in-memory/persistent backends.
-}

module Headroom.IO.KVStore
  ( -- * Data Types
    KVStore(..)
  , ValueCodec(..)
  , ValueRepr
  , StoreKey(..)
    -- * Helper Functions
  , storeKey
  )
where

import           RIO


---------------------------------  DATA TYPES  ---------------------------------

-- | Represents key-value store. Whether this store is persistent or not is
-- dependend on the concrete implementation. Operation over keys is done in
-- type-safe manner by declaring them using 'Storekey'. Store can store any
-- value type for which the instance of 'ValueCodec' is defined.
class KVStore s where

  -- | Gets value for given key from the store (if exists).
  getValue :: forall a m. (ValueCodec s a, MonadIO m)
           => s           -- ^ instance of store
           -> StoreKey a  -- ^ key
           -> m (Maybe a) -- ^ value (if found)

  setValue :: forall a m. (ValueCodec s a, MonadIO m)
           => s          -- ^ instance of store
           -> StoreKey a -- ^ key
           -> a          -- ^ value to store
           -> m ()       -- ^ operation result


-- | Type class providing support to decode/encode given value for given
-- 'KVStore' instance.
class ValueCodec s a where

  -- | Encodes value into type recognized by given 'KVStore' instance.
  encodeValue :: a           -- ^ value to encode
              -> ValueRepr s -- ^ encoded representation

  -- | Decodes value from type recognized by given 'KVStore' instance.
  decodeValue :: ValueRepr s -- ^ encoded representation
              -> Maybe a     -- ^ decoded value


-- | Representation of encoded store value, specific to the given 'KVStore'
-- instance. Main point is that each 'KVStore' instance might need to
-- encode/decode given type into some internal representation. Using this type
-- family, it's possible to define such representation in type safe manner.
type family ValueRepr s


-- | Type safe representation of store key.
newtype StoreKey a = CacheEntry Text deriving (Eq, Show)


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Constructor function for 'StoreKey'.
storeKey :: Text       -- ^ name of the key
         -> StoreKey a -- ^ type safe representation
storeKey = CacheEntry
