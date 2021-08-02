{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.IO.Cache.SQLiteSpec
  ( spec
  )
where

import           Headroom.IO.Cache
import           Headroom.IO.Cache.SQLite
import           RIO
import           RIO.FilePath                        ( (</>) )
import qualified RIO.Text                           as T
import           Test.Hspec


spec :: Spec
spec = do

  describe "SQLiteCache instance of Cache" $ do
    it "reads and writes values from/to cache" $ do
      withSystemTempDirectory "sqlite-cache" $ \dir -> do
        let cache   = SQLiteCache . T.pack $ dir </> "test-db.sqlite"
            nameKey = cacheKey @Text "name"
            yearKey = cacheKey @Int "year"
        (maybeName, maybeYear) <- withCache cache $ do
          name <- getValue nameKey
          _    <- setValue yearKey 41
          _    <- setValue yearKey 42
          year <- getValue yearKey
          pure (name, year)
        maybeName `shouldBe` Nothing
        maybeYear `shouldBe` Just 42
