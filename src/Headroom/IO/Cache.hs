{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Headroom.IO.Cache
Description : Support for Cache implementations
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides abstract key/value cache representation that allows to
implement specific in-memory/persistent backends.
-}

module Headroom.IO.Cache
  ( -- * Data Types
    Cache(..)
  , CacheT
  , CacheKey(..)
  , ValueCodec(..)
  , ValueRepr
    -- * Helper Functions
  , cacheKey
  , withCache
  )
where

import           RIO


---------------------------------  DATA TYPES  ---------------------------------

-- | Represents key/value cache. Whether this cache is persistent or not is
-- dependend on the concrete implementation. Operation over keys is done in
-- type-safe manner by declaring them using 'CacheKey'. Cache can store any
-- value type for which the instance of 'ValueCodec' is defined.
class Cache c where

  -- | Gets value for given key from the cache (if exists).
  getValue :: forall a m. (ValueCodec c a, MonadIO m)
           => CacheKey a           -- ^ key
           -> CacheT c m (Maybe a) -- ^ value (if found)

  -- | Sets value for given key in the cache. If the value already exists, it's
  -- replaced by the new value.
  setValue :: forall a m. (ValueCodec c a, MonadIO m)
           => CacheKey a    -- ^ key
           -> a             -- ^ value to store
           -> CacheT c m () -- ^ operation result


-- | Transformer monad type for cache operations.
type CacheT = ReaderT


-- | Type class providing support to decode/encode given value for given 'Cache'
-- instance.
class ValueCodec c a where

  -- | Encodes value into type recognized by given 'Cache' instance.
  encodeValue :: a           -- ^ value to encode
              -> ValueRepr c -- ^ encoded representation

  -- | Decodes value from type recognized by given 'Cache' instance.
  decodeValue :: ValueRepr c -- ^ encoded representation
              -> Maybe a     -- ^ decoded value


-- | Representation of encoded cache value, specific to the given 'Cache'
-- instance. Main point is that each 'Cache' instance might need to
-- encode/decode given type into some internal representation. Using this type
-- family, it's possible to define such representation in type safe manner.
type family ValueRepr c


-- | Type safe representation of cache key.
newtype CacheKey a = CacheEntry Text deriving (Eq, Show)


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Constructor function for 'CacheKey'.
cacheKey :: Text       -- ^ name of the key
         -> CacheKey a -- ^ type safe representation
cacheKey = CacheEntry


-- | Runs the monadic cache operation using the given 'Cache' instance.
withCache :: c            -- ^ 'Cache' instance to use
          -> CacheT c m a -- ^ operations over cache
          -> m a          -- ^ operation result
withCache cache op = runReaderT op cache
