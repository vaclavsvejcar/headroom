{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.Template.TemplateRefSpec
  ( spec
  )
where


import           Headroom.Template.Mustache          ( Mustache )
import           Headroom.Template.TemplateRef
import           RIO
import qualified RIO.List                           as L
import           Test.Hspec
import           Text.URI.QQ                         ( uri )


spec :: Spec
spec = do

  describe "mkTemplateRef" $ do
    it "creates valid reference to local Haskell template" $ do
      let raw      = "/path/to/some/haskell.mustache"
          expected = LocalTemplateRef "/path/to/some/haskell.mustache"
      mkTemplateRef @Mustache raw `shouldBe` Just expected

    it "creates valid reference to HTTP Haskell template" $ do
      let raw      = "http://foo/haskell.mustache"
          expected = UriTemplateRef [uri|http://foo/haskell.mustache|]
      mkTemplateRef @Mustache raw `shouldBe` Just expected

    it "throws error if URI is valid but protocol is not supported" $ do
      let raw = "foo://foo/haskell.mustache"
      mkTemplateRef @Mustache raw `shouldThrow` \case
        (UnsupportedUriProtocol _ _) -> True
        _                            -> False

    it "throws error if URI is valid but file type is not supported" $ do
      let raw = "http://foo/bar.mustache"
      mkTemplateRef @Mustache raw `shouldThrow` \case
        (UnrecognizedTemplateName _) -> True
        _                            -> False


  describe "renderRef" $ do
    it "renders local template reference to human friendly text" $ do
      let sample   = LocalTemplateRef "/path/to/some/haskell.mustache"
          expected = "/path/to/some/haskell.mustache"
      renderRef sample `shouldBe` expected

    it "renders URI template reference to human friendly text" $ do
      let sample   = UriTemplateRef [uri|http://foo/haskell.mustache|]
          expected = "http://foo/haskell.mustache"
      renderRef sample `shouldBe` expected


  describe "Ord instance for TemplateRef" $ do
    it "should properly order records" $ do
      let sample =
            [ UriTemplateRef [uri|http://foo/haskell.mustache|]
            , LocalTemplateRef "/path/to/some/haskell.mustache"
            ]
          expected =
            [ LocalTemplateRef "/path/to/some/haskell.mustache"
            , UriTemplateRef [uri|http://foo/haskell.mustache|]
            ]
      L.sort sample `shouldBe` expected
