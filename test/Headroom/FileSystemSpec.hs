{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.FileSystemSpec
  ( spec
  )
where

import           Headroom.Data.Regex                 ( re )
import           Headroom.FileSystem
import           RIO
import           RIO.FilePath                        ( (</>) )
import           RIO.List                            ( sort )
import qualified RIO.List                           as L
import           Test.Hspec


spec :: Spec
spec = do
  describe "fileExtension" $ do
    it "returns file extension for valid file path" $ do
      fileExtension "/some/path/to/file.txt" `shouldBe` Just "txt"

    it "returns nothing for invalid file path" $ do
      fileExtension "/some/nonsense/path" `shouldBe` Nothing


  describe "findFiles" $ do
    it "recursively finds files filtered by given predicate" $ do
      let path      = "test-data" </> "test-traverse"
          predicate = ("b.html" `L.isSuffixOf`)
          expected  = ["test-data" </> "test-traverse" </> "foo" </> "b.html"]
      sort <$> findFiles path predicate `shouldReturn` sort expected


  describe "findFilesByExts" $ do
    it "recursively finds files filtered by its file extension" $ do
      let path     = "test-data" </> "test-traverse"
          exts     = ["xml"]
          expected = ["test-data" </> "test-traverse" </> "foo" </> "test.xml"]
      sort <$> findFilesByExts path exts `shouldReturn` sort expected


  describe "listFiles" $ do
    it "recursively finds all files in directory" $ do
      let path = "test-data" </> "test-traverse"
          expected =
            [ "test-data" </> "test-traverse" </> "a.html"
            , "test-data" </> "test-traverse" </> "foo" </> "b.html"
            , "test-data" </> "test-traverse" </> "foo" </> "test.xml"
            , "test-data" </> "test-traverse" </> "foo" </> "bar" </> "c.html"
            ]
      sort <$> listFiles path `shouldReturn` sort expected

    it "returns file if file path is passed as argument" $ do
      let path = "test-data" </> "test-traverse" </> "a.html"
      sort <$> listFiles path `shouldReturn` [path]


  describe "excludePaths" $ do
    it "excludes paths matching selected pattern from input list" $ do
      let patterns = [[re|\.stack-work|], [re|remove\.txt|]]
          sample =
            [ "/foo/bar/.stack-work/xx"
            , "/hello/world"
            , "foo/bar/remove.txt"
            , "xx/yy"
            ]
          expected = ["/hello/world", "xx/yy"]
      excludePaths patterns sample `shouldBe` expected
