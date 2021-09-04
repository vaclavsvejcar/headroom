{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Headroom.UpdaterSpec
  ( spec
  )
where

import           Data.Aeson                          ( Value )
import qualified Data.Aeson                         as A
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Lens                  ( suffixLenses
                                                     , suffixLensesFor
                                                     )
import           Headroom.IO.Network                 ( Network(..)
                                                     , NetworkError(..)
                                                     )
import           Headroom.Meta.Version               ( pvp )
import           Headroom.Updater
import           RIO
import qualified RIO.ByteString                     as B
import qualified RIO.ByteString.Lazy                as BL
import           RIO.FilePath                        ( (</>) )
import           RIO.Partial                         ( fromJust )
import           Test.Hspec


data TestEnv = TestEnv
  { envNetwork :: Network (RIO TestEnv)
  }

suffixLenses ''TestEnv
suffixLensesFor ["nDownloadContent"] ''Network

instance Has (Network (RIO TestEnv)) TestEnv where
  hasLens = envNetworkL


spec :: Spec
spec = do
  let testFile = "test-data" </> "updater" </> "github-resp.json"

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
env0 = TestEnv { envNetwork = Network { nDownloadContent = undefined } }
