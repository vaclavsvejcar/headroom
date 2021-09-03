{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.UpdaterSpec
  ( spec
  )
where

import           Data.Aeson                          ( Value )
import qualified Data.Aeson                         as A
import           Headroom.Meta.Version               ( pvp )
import           Headroom.Updater
import           RIO
import qualified RIO.ByteString.Lazy                as BL
import           RIO.FilePath                        ( (</>) )
import           RIO.Partial                         ( fromJust )
import           Test.Hspec


spec :: Spec
spec = do

  describe "parseLatestVersion" $ do
    it "parses latest version from raw JSON input" $ do
      raw    <- BL.readFile $ "test-data" </> "updater" </> "github-resp.json"
      actual <- parseLatestVersion (fromJust . A.decode @Value $ raw)
      actual `shouldBe` [pvp|0.4.2.0|]
