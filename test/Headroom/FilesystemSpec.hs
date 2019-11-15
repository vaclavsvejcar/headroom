{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.FilesystemSpec
  ( spec
  )
where

import           Headroom.Filesystem
import           Test.Hspec
import           RIO
import           RIO.List                       ( sort )

spec :: Spec
spec = do
  describe "traverseDir" $ do
    it "recursively finds all files in directory" $ do
      filePaths <- traverseDir "test-data/test-traverse/"
      let expected =
            [ "test-data/test-traverse/a.html"
            , "test-data/test-traverse/foo/b.html"
            , "test-data/test-traverse/foo/bar/c.html"
            ]
      sort filePaths `shouldBe` sort expected
