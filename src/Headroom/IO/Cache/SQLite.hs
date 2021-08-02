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
Module      : Headroom.IO.Cache.SQLite
Description : /SQLite/ backend for cache support
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Persistent 'Cache' implementation using /SQLite/ as backend store. This
implementation is quite crude and definitely not great performance-wise, but
given the intended use it should be good enough for now.
-}

module Headroom.IO.Cache.SQLite
  ( SQLiteCache(..)
  )
where

import           Database.Persist
import           Database.Persist.Sqlite             ( runMigrationSilent
                                                     , runSqlite
                                                     )
import           Database.Persist.TH
import qualified Headroom.Data.Text                 as T
import           Headroom.IO.Cache                   ( Cache(..)
                                                     , CacheKey(..)
                                                     , CacheT
                                                     , ValueCodec(..)
                                                     , ValueRepr
                                                     )
import           RIO
import qualified RIO.Text                           as T


------------------------------  TEMPLATE HASKELL  ------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CacheStore
  Id Text
  value Text
  deriving Show
|]


---------------------------------  DATA TYPES  ---------------------------------

-- | Persistent cache implementation using /SQLite/ as backend store.
newtype SQLiteCache = SQLiteCache Text deriving (Eq, Show)


instance Cache SQLiteCache where
  getValue = getValue'
  setValue = setValue'


type instance ValueRepr SQLiteCache = Text


instance ValueCodec SQLiteCache Int where
  encodeValue = T.pack . show
  decodeValue = T.read


instance ValueCodec SQLiteCache Text where
  encodeValue = id
  decodeValue = Just


------------------------------  PRIVATE FUNCTIONS  -----------------------------

getValue' :: (ValueCodec SQLiteCache a, MonadIO m)
          => CacheKey a
          -> CacheT SQLiteCache m (Maybe a)
getValue' (CacheEntry key) = do
  SQLiteCache connPath <- ask
  liftIO . runSqlite connPath $ do
    _          <- runMigrationSilent migrateAll
    maybeValue <- get $ CacheStoreKey key
    case maybeValue of
      Just (CacheStore v) -> pure . decodeValue @SQLiteCache $ v
      Nothing             -> pure Nothing


setValue' :: (ValueCodec SQLiteCache a, MonadIO m)
          => CacheKey a
          -> a
          -> CacheT SQLiteCache m ()
setValue' (CacheEntry key) value = do
  SQLiteCache connPath <- ask
  let newValue = encodeValue @SQLiteCache value
  liftIO . runSqlite connPath $ do
    _ <- runMigrationSilent migrateAll
    repsert (CacheStoreKey key) (CacheStore newValue)
