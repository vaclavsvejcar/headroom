{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Headroom.Configuration.CompatSpec
  ( spec
  )
where

import           Headroom.Configuration.Compat
import           Headroom.Meta.Version          ( Version(..)
                                                , pvp
                                                )
import           RIO
import           Test.Hspec


spec :: Spec
spec = do
  describe "checkCompatibility" $ do
    it "passes when config version is compatible" $ do
      let yaml     = "version: 0.4.0.0"
          versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
      checkCompatibility versions yaml `shouldBe` Just ()

    it "fails when config version is not compatible" $ do
      let yaml     = "version: 0.2.1.0"
          versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
      let err (UnsupportedVersion [Version 0 4 0 0] (Version 0 2 1 0)) = True
          err _ = False
      checkCompatibility versions yaml `shouldThrow` err

    it "fails when config version cannot be determined" $ do
      let yaml     = ""
          versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
      let err CannotParseVersion = True
          err _                  = False
      checkCompatibility versions yaml `shouldThrow` err
