{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Headroom.Template.TemplateRefSpec
  ( spec
  )
where


import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Template.TemplateRef
import           RIO
import           Test.Hspec
import           Text.URI.QQ                         ( uri )


spec :: Spec
spec = do

  describe "mkTemplateRef" $ do
    it "creates valid reference to local Haskell template" $ do
      let raw      = "/path/to/some/haskell.mustache"
          expected = TemplateRef
            { trFileType = Haskell
            , trSource   = LocalTemplateSource "/path/to/some/haskell.mustache"
            }
      mkTemplateRef raw `shouldBe` Just expected

    it "creates valid reference to HTTP Haskell template" $ do
      let raw      = "http://foo/haskell.mustache"
          expected = TemplateRef
            { trFileType = Haskell
            , trSource   = UriTemplateSource [uri|http://foo/haskell.mustache|]
            }
      mkTemplateRef raw `shouldBe` Just expected

    it "throws error if URI is valid but protocol is not supported" $ do
      let raw = "foo://foo/haskell.mustache"
      mkTemplateRef raw `shouldThrow` \case
        (UnsupportedUriProtocol _ _) -> True
        _                            -> False

