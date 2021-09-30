{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Headroom.UpdaterSpec
  ( spec
  )
where

import           Data.Aeson                          ( Value )
import qualified Data.Aeson                         as A
import           Data.String.Interpolate             ( i )
import           Headroom.Configuration.GlobalConfig ( UpdaterConfig(..) )
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Lens                  ( suffixLenses
                                                     , suffixLensesFor
                                                     )
import           Headroom.IO.KVStore                 ( KVStore(..) )
import           Headroom.IO.Network                 ( Network(..)
                                                     , NetworkError(..)
                                                     )
import           Headroom.Meta                       ( buildVersion )
import           Headroom.Meta.Version               ( printVersionP
                                                     , pvp
                                                     )
import           Headroom.Updater
import           RIO
import qualified RIO.ByteString                     as B
import qualified RIO.ByteString.Lazy                as BL
import           RIO.FilePath                        ( (</>) )
import           RIO.Partial                         ( fromJust )
import           Test.Hspec



data TestEnv = TestEnv
  { envKVStore       :: KVStore (RIO TestEnv)
  , envNetwork       :: Network (RIO TestEnv)
  , envUpdaterConfig :: UpdaterConfig
  }

suffixLenses ''TestEnv
suffixLensesFor ["nDownloadContent"] ''Network

instance Has (KVStore (RIO TestEnv)) TestEnv where
  hasLens = envKVStoreL

instance Has (Network (RIO TestEnv)) TestEnv where
  hasLens = envNetworkL

instance Has UpdaterConfig TestEnv where
  hasLens = envUpdaterConfigL


spec :: Spec
spec = do
  let testFile = "test-data" </> "updater" </> "github-resp.json"

  describe "checkUpdates" $ do
    it "returns Nothing if current version is the latest" $ do
      let json = [i|{"name": "#{printVersionP buildVersion}"}|]
          env =
            env0
              & (envKVStoreL .~ kvStore')
              & (envNetworkL . nDownloadContentL .~ nDownloadContent')
              & (envUpdaterConfigL .~ updaterConfig')
          nDownloadContent' = const . pure $ json
          updaterConfig'    = UpdaterConfig True 1
          kvStore'          = KVStore { kvGetValue = const . pure $ Nothing
                                      , kvPutValue = const . const . pure $ ()
                                      }
      actual <- runRIO env checkUpdates
      actual `shouldBe` Nothing


  describe "fetchLatestVersion" $ do
    it "gets latest version info" $ do
      raw <- B.readFile testFile
      let env = env0 & (envNetworkL . nDownloadContentL .~ nDownloadContent')
          nDownloadContent' = const . pure $ raw
      actual <- runRIO env fetchLatestVersion
      actual `shouldBe` [pvp|0.4.2.0|]

    it "returns error if version cannot be fetched" $ do
      let env = env0 & (envNetworkL . nDownloadContentL .~ nDownloadContent')
          nDownloadContent' = \uri -> throwM $ ConnectionFailure uri "error"
      runRIO env fetchLatestVersion `shouldThrow` \case
        (CannotDetectVersion _) -> True


  describe "parseLatestVersion" $ do
    it "parses latest version from raw JSON input" $ do
      raw    <- BL.readFile testFile
      actual <- parseLatestVersion (fromJust . A.decode @Value $ raw)
      actual `shouldBe` [pvp|0.4.2.0|]


env0 :: TestEnv
env0 = TestEnv
  { envKVStore = KVStore { kvGetValue = undefined, kvPutValue = undefined }
  , envNetwork = Network { nDownloadContent = undefined }
  , envUpdaterConfig = undefined
  }
