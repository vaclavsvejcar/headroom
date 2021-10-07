{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.IO.KvStoreSpec
  ( spec
  )
where

import           Headroom.IO.KVStore
import           RIO
import           RIO.FilePath                        ( (</>) )
import qualified RIO.Text                           as T
import           RIO.Time
import           Test.Hspec


spec :: Spec
spec = do

  describe "SQLite store" $ do
    it "reads and writes values from/to store" $ do
      withSystemTempDirectory "sqlite-kvstore" $ \dir -> do
        let path         = StorePath . T.pack $ dir </> "test-db.sqlite"
            fstKey       = valueKey @Text "fst-key"
            sndKey       = valueKey @Text "snd-key"
            KVStore {..} = sqliteKVStore path
        maybeFst <- kvGetValue fstKey
        _        <- kvPutValue sndKey "foo"
        _        <- kvPutValue sndKey "bar"
        maybeSnd <- kvGetValue sndKey
        maybeFst `shouldBe` Nothing
        maybeSnd `shouldBe` Just "bar"


  describe "In-memory store" $ do
    it "reads and writes values from/to store" $ do
      let fstKey = valueKey @Text "fst-key"
          sndKey = valueKey @Text "snd-key"
      KVStore {..} <- inMemoryKVStore
      maybeFst     <- kvGetValue fstKey
      _            <- kvPutValue sndKey "foo"
      _            <- kvPutValue sndKey "bar"
      maybeSnd     <- kvGetValue sndKey
      maybeFst `shouldBe` Nothing
      maybeSnd `shouldBe` Just "bar"


  describe "ValueCodec type class" $ do
    it "has working instance for Text" $ do
      let sample = "The Cake is a Lie"
      decodeValue @Text (encodeValue sample) `shouldBe` Just sample

    it "has working instance for UTCTime" $ do
      sample <- getCurrentTime
      decodeValue @UTCTime (encodeValue sample) `shouldBe` Just sample

