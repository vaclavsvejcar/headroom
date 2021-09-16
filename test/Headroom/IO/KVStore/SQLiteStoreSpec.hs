{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.IO.KVStore.SQLiteStoreSpec
  ( spec
  )
where

import           Headroom.IO.KVStore
import           Headroom.IO.KVStore.SQLiteStore
import           RIO
import           RIO.FilePath                        ( (</>) )
import qualified RIO.Text                           as T
import           Test.Hspec


spec :: Spec
spec = do

  describe "SQLiteStore instance of KVStore" $ do
    it "reads and writes values from/to store" $ do
      withSystemTempDirectory "sqlite-kvstore" $ \dir -> do
        let store   = SQLiteStore . T.pack $ dir </> "test-db.sqlite"
            nameKey = storeKey @Text "name"
            yearKey = storeKey @Int "year"
        maybeName <- getValue store nameKey
        _         <- setValue store yearKey 41
        _         <- setValue store yearKey 42
        maybeYear <- getValue store yearKey
        maybeName `shouldBe` Nothing
        maybeYear `shouldBe` Just 42
