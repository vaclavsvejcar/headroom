{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FilesystemSpec
  ( spec
  )
where

import           Headroom.Filesystem
import           Test.Hspec
import           RIO
import           RIO.List                       ( sort )
import qualified RIO.List                      as L


spec :: Spec
spec = do
  describe "findFiles" $ do
    it "recursively finds files filtered by given predicate" $ do
      files <- findFiles "test-data/test-traverse/" ("b.html" `L.isSuffixOf`)
      let expected = ["test-data/test-traverse/foo/b.html"]
      files `shouldBe` expected

  describe "findFilesByExts" $ do
    it "recursively finds files filtered by its file extension" $ do
      files <- findFilesByExts "test-data/test-traverse/" ["xml"]
      let expected = ["test-data/test-traverse/foo/test.xml"]
      files `shouldBe` expected

  describe "listFiles" $ do
    it "recursively finds all files in directory" $ do
      filePaths <- listFiles "test-data/test-traverse/"
      let expected =
            [ "test-data/test-traverse/a.html"
            , "test-data/test-traverse/foo/b.html"
            , "test-data/test-traverse/foo/test.xml"
            , "test-data/test-traverse/foo/bar/c.html"
            ]
      sort filePaths `shouldBe` sort expected

    it "returns file if file path is passed as argument" $ do
      filePaths <- listFiles "test-data/test-traverse/a.html"
      let expected = ["test-data/test-traverse/a.html"]
      sort filePaths `shouldBe` sort expected
