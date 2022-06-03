{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.IO.KVStore
-- Description : Key-value persistent store
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is really simple /key-value/ persistent store that uses /SQLite/ as a
-- backend. Main goal is to provide /type-safe/ way how to define value keys, that
-- can be later used to set/put the actual value into the store.
module Headroom.IO.KVStore (
    -- * Type Aliases
      GetValueFn
    , PutValueFn
    , KVStore (..)

      -- * Type Classes
    , ValueCodec (..)

      -- * Data Types
    , ValueKey (..)
    , StorePath (..)

      -- * Public Functions
    , inMemoryKVStore
    , sqliteKVStore
    , valueKey
) where

import Database.Persist (
    PersistStoreRead (..)
    , PersistStoreWrite (..)
 )
import Database.Persist.Sqlite (
    runMigrationSilent
    , runSqlite
 )
import Database.Persist.TH (
    mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
 )
import RIO
import qualified RIO.Map as M
import qualified RIO.Text as T
import RIO.Time (
    UTCTime
    , defaultTimeLocale
    , formatTime
    , parseTimeM
 )

------------------------------  TEMPLATE HASKELL  ------------------------------

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
StoreRecord
  Id Text
  value Text
  deriving Show
|]

--------------------------------  TYPE ALIASES  --------------------------------

-- | Gets the value for given 'ValueKey' from the store.
type GetValueFn m =
    forall a.
    (ValueCodec a) =>
    -- | key for the value
    ValueKey a ->
    -- | value (if found)
    m (Maybe a)

-- | Puts the value for given 'ValueKey' into the store.
type PutValueFn m =
    forall a.
    (ValueCodec a) =>
    -- | key for the value
    ValueKey a ->
    -- | value to put into store
    a ->
    -- | operation result
    m ()

-----------------------------  POLYMORPHIC RECORD  -----------------------------

-- | /Polymorphic record/ composed of /key-value/ store operations, allowing to
-- abstract over concrete implementation without (ab)using /type classes/.
data KVStore m = KVStore
    { kvGetValue :: GetValueFn m
    , kvPutValue :: PutValueFn m
    }

-- | Constructs persistent instance of 'KVStore' that uses /SQLite/ as a backend.
sqliteKVStore ::
    MonadIO m =>
    -- | path of the store location
    StorePath ->
    -- | store instance
    KVStore m
sqliteKVStore sp =
    KVStore{kvGetValue = getValueSQLite sp, kvPutValue = putValueSQLite sp}

-- | Constructs non-persistent in-memory instance of 'KVStore'.
inMemoryKVStore :: MonadIO m => m (KVStore m)
inMemoryKVStore = do
    ref <- newIORef M.empty
    pure
        KVStore
            { kvGetValue = getValueInMemory ref
            , kvPutValue = putValueInMemory ref
            }

--------------------------------  TYPE CLASSES  --------------------------------

-- | Represents way how to encode/decode concrete types into textual
-- representation used by the store to hold values.
class ValueCodec a where
    -- | Encodes value into textual representation.
    encodeValue ::
        -- | value to encode
        a ->
        -- | textual representation
        Text

    -- | Decodes value from textual representation.
    decodeValue ::
        -- | value to decode
        Text ->
        -- | decoded value (if available)
        Maybe a

instance ValueCodec Text where
    encodeValue = id
    decodeValue = Just

instance ValueCodec UTCTime where
    encodeValue = T.pack . formatTime defaultTimeLocale "%FT%T%Q"
    decodeValue = parseTimeM True defaultTimeLocale "%FT%T%Q" . T.unpack

---------------------------------  DATA TYPES  ---------------------------------

-- | /Type-safe/ representation of the key for specific value.
newtype ValueKey a = ValueKey Text deriving (Eq, Show)

-- | Constructor function for 'ValueKey'.
valueKey :: Text -> ValueKey a
valueKey = ValueKey

-- | Path to the store (e.g. path of the /SQLite/ database on filesystem).
newtype StorePath = StorePath Text deriving (Eq, Show)

------------------------------  PRIVATE FUNCTIONS  -----------------------------

getValueInMemory :: MonadIO m => IORef (Map Text Text) -> GetValueFn m
getValueInMemory ref (ValueKey key) = do
    storeMap <- readIORef ref
    pure $ M.lookup key storeMap >>= decodeValue

putValueInMemory :: MonadIO m => IORef (Map Text Text) -> PutValueFn m
putValueInMemory ref (ValueKey key) value = do
    modifyIORef ref $ M.insert key (encodeValue value)
    pure ()

getValueSQLite :: MonadIO m => StorePath -> GetValueFn m
getValueSQLite (StorePath path) (ValueKey key) = do
    liftIO . runSqlite path $ do
        _ <- runMigrationSilent migrateAll
        maybeValue <- get $ StoreRecordKey key
        case maybeValue of
            Just (StoreRecord v) -> pure . decodeValue $ v
            Nothing -> pure Nothing

putValueSQLite :: MonadIO m => StorePath -> PutValueFn m
putValueSQLite (StorePath path) (ValueKey key) value = do
    liftIO . runSqlite path $ do
        _ <- runMigrationSilent migrateAll
        repsert (StoreRecordKey key) (StoreRecord $ encodeValue value)
