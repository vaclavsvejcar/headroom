{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ConfigurationSpec
  ( spec
  )
where

import           Data.Monoid                    ( Last(..) )
import           Headroom.Configuration
import           Headroom.Types                 ( ApplicationError(..)
                                                , Configuration(..)
                                                , ConfigurationError(..)
                                                , PartialConfiguration(..)
                                                , RunMode(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import           Test.Hspec
import           Test.Utils                     ( matchesException )


spec :: Spec
spec = do
  describe "makeConfiguration" $ do
    it "should make Configuration from valid PartialConfiguration" $ do
      let pc = defaultPartialConfiguration
            { pcSourcePaths   = Last $ Just ["foo/bar"]
            , pcTemplatePaths = Last $ Just ["foo/bar"]
            }
          expected = Configuration { cRunMode       = Add
                                   , cSourcePaths   = ["foo/bar"]
                                   , cTemplatePaths = ["foo/bar"]
                                   , cVariables     = HM.fromList []
                                   }
      makeConfiguration pc `shouldBe` Just expected

    it "should fail for incomplete PartialConfiguration" $ do
      let pc    = defaultPartialConfiguration
          check = \case
            Just (ConfigurationError NoSourcePaths) -> True
            _ -> False
      makeConfiguration pc `shouldSatisfy` matchesException check
