{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.ConfigurationSpec
  ( spec
  )
where

import           Headroom.Configuration
import           Headroom.Types                 ( ApplicationError(..)
                                                , Configuration(..)
                                                , ConfigurationError(..)
                                                , HeaderConfig(..)
                                                , HeadersConfig(..)
                                                , HeadersConfig(..)
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
      makeConfiguration testPartialConfiguration
        `shouldBe` Just testConfiguration

    it "should fail for incomplete PartialConfiguration" $ do
      let pc    = defaultPartialConfiguration
          check = \case
            Just (ConfigurationError NoSourcePaths) -> True
            _ -> False
      makeConfiguration pc `shouldSatisfy` matchesException check

testPartialConfiguration :: PartialConfiguration
testPartialConfiguration = defaultPartialConfiguration
  { pcSourcePaths    = pure ["foo/bar"]
  , pcTemplatePaths  = pure ["foo/bar"]
  , pcLicenseHeaders = defaultPartialHeadersConfig
  }

testConfiguration :: Configuration
testConfiguration = Configuration
  { cRunMode        = Add
  , cSourcePaths    = ["foo/bar"]
  , cTemplatePaths  = ["foo/bar"]
  , cVariables      = HM.fromList []
  , cLicenseHeaders = HeadersConfig
                        { hscHaskell = HeaderConfig { hcFileExtensions = ["hs"]
                                                    , hcPutAfter       = []
                                                    , hcStartsWith     = "{-"
                                                    , hcEndsWith       = "-}"
                                                    }
                        , hscHtml    = HeaderConfig
                                         { hcFileExtensions = ["htm", "html"]
                                         , hcPutAfter       = []
                                         , hcStartsWith     = "<!--"
                                         , hcEndsWith       = "-->"
                                         }
                        }
  }
