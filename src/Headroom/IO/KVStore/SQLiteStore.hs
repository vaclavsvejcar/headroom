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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Headroom.IO.KVStore.SQLiteStore
Description : /SQLite/ backend for key-value store support
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Persistent 'KVStore' implementation using /SQLite/ as backend store. This
implementation is quite crude and definitely not great performance-wise, but
given the intended use it should be good enough for now.
-}

module Headroom.IO.KVStore.SQLiteStore
  ( SQLiteStore(..)
  )
where

import           Database.Persist
import           Database.Persist.Sqlite             ( runMigrationSilent
                                                     , runSqlite
                                                     )
import           Database.Persist.TH
import qualified Headroom.Data.Text                 as T
import           Headroom.IO.KVStore                 ( KVStore(..)
                                                     , StoreKey(..)
                                                     , ValueCodec(..)
                                                     , ValueRepr
                                                     )
import           RIO
import qualified RIO.Text                           as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CacheStore
  Id Text
  value Text
  deriving Show
|]

-- | Persistent implementation of 'KVStore' that uses /SQLite/ as a backend.
newtype SQLiteStore = SQLiteStore Text deriving (Eq, Show)

instance KVStore SQLiteStore where
  getValue = getValue'
  setValue = setValue'

type instance ValueRepr SQLiteStore = Text

instance ValueCodec SQLiteStore Int where
  encodeValue = T.pack . show
  decodeValue = T.read


instance ValueCodec SQLiteStore Text where
  encodeValue = id
  decodeValue = Just


getValue' :: (ValueCodec SQLiteStore a, MonadIO m)
          => SQLiteStore
          -> StoreKey a
          -> m (Maybe a)
getValue' (SQLiteStore conn) (CacheEntry key) = do
  liftIO . runSqlite conn $ do
    _          <- runMigrationSilent migrateAll
    maybeValue <- get $ CacheStoreKey key
    case maybeValue of
      Just (CacheStore v) -> pure . decodeValue @SQLiteStore $ v
      Nothing             -> pure Nothing


setValue' :: (ValueCodec SQLiteStore a, MonadIO m)
          => SQLiteStore
          -> StoreKey a
          -> a
          -> m ()
setValue' (SQLiteStore conn) (CacheEntry key) value = do
  let newValue = encodeValue @SQLiteStore value
  liftIO . runSqlite conn $ do
    _ <- runMigrationSilent migrateAll
    repsert (CacheStoreKey key) (CacheStore newValue)
