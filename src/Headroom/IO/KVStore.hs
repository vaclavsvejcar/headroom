{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Headroom.IO.KVStore
Description : Key-value persistent store
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This is really simple /key-value/ persistent store that uses /SQLite/ as a
backend. Main goal is to provide /type-safe/ way how to define value keys, that
can be later used to set/put the actual value into the store.
-}

module Headroom.IO.KVStore
  ( -- * Type Aliases
    GetValueFn
  , PutValueFn
  , KVStore(..)
    -- * Type Classes
  , ValueCodec(..)
    -- * Data Types
  , ValueKey(..)
  , StorePath(..)
    -- * Public Functions
  , mkKVStore
  , valueKey
  , getValue
  , putValue
  )
where

import           Database.Persist                    ( PersistStoreRead(..)
                                                     , PersistStoreWrite(..)
                                                     )
import           Database.Persist.Sqlite             ( runMigrationSilent
                                                     , runSqlite
                                                     )
import           Database.Persist.TH                 ( mkMigrate
                                                     , mkPersist
                                                     , persistLowerCase
                                                     , share
                                                     , sqlSettings
                                                     )
import           RIO
import qualified RIO.Text                           as T
import           RIO.Time                            ( UTCTime
                                                     , defaultTimeLocale
                                                     , formatTime
                                                     , parseTimeM
                                                     )


------------------------------  TEMPLATE HASKELL  ------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoreRecord
  Id Text
  value Text
  deriving Show
|]


--------------------------------  TYPE ALIASES  --------------------------------

-- | Gets the value for given 'ValueKey' from the store.
type GetValueFn m
  =  forall a
   . (ValueCodec a)
  => ValueKey a  -- ^ key for the value
  -> m (Maybe a) -- ^ value (if found)


-- | Puts the value for given 'ValueKey' into the store.
type PutValueFn m
  =  forall a
   . (ValueCodec a)
  => ValueKey a -- ^ key for the value
  -> a          -- ^ value to put into store
  -> m ()       -- ^ operation result


-----------------------------  POLYMORPHIC RECORD  -----------------------------

-- | /Polymorphic record/ composed of /key-value/ store operations, allowing to
-- abstract over concrete implementation without (ab)using /type classes/.
data KVStore m = KVStore
  { kvGetValue :: GetValueFn m
  , kvPutValue :: PutValueFn m
  }


-- | Constructs the default 'KVStore' that uses /SQLite/ as a backend.
mkKVStore :: MonadIO m
          => StorePath -- ^ path of the store location
          -> KVStore m -- ^ store instance
mkKVStore sp = KVStore { kvGetValue = getValue sp, kvPutValue = putValue sp }

--------------------------------  TYPE CLASSES  --------------------------------

-- | Represents way how to encode/decode concrete types into textual
-- representation used by the store to hold values.
class ValueCodec a where

  -- | Encodes value into textual representation.
  encodeValue :: a    -- ^ value to encode
              -> Text -- ^ textual representation


  -- | Decodes value from textual representation.
  decodeValue :: Text    -- ^ value to decode
              -> Maybe a -- ^ decoded value (if available)


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


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'GetValueFn' that gets value from /SQLite/.
getValue :: MonadIO m => StorePath -> GetValueFn m
getValue (StorePath path) (ValueKey key) = do
  liftIO . runSqlite path $ do
    _          <- runMigrationSilent migrateAll
    maybeValue <- get $ StoreRecordKey key
    case maybeValue of
      Just (StoreRecord v) -> pure . decodeValue $ v
      Nothing              -> pure Nothing


-- | Implementation of 'PutValueFn' that puts value to /SQLite/.
putValue :: MonadIO m => StorePath -> PutValueFn m
putValue (StorePath path) (ValueKey key) value = do
  liftIO . runSqlite path $ do
    _ <- runMigrationSilent migrateAll
    repsert (StoreRecordKey key) (StoreRecord $ encodeValue value)
