{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileSupportSpec
  ( spec
  )
where

import           Headroom.FileSupport
import           Headroom.Types                 ( FileInfo(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                )
import           RIO
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashMap                   as HM
import           Test.Hspec
import           Text.Regex.PCRE.Light          ( compile )
import           Text.Regex.PCRE.Light.Char8    ( utf8 )


spec :: Spec
spec = do
  let samplesDir = "test-data" </> "code-samples"

  describe "addHeader" $ do
    let fileInfo config = FileInfo Haskell config Nothing HM.empty
    it "adds header at the beginning of text" $ do
      let info     = fileInfo $ HeaderConfig ["hs"] [] "{-|" "-}"
          header   = "HEADER"
          sample   = "1\n2\nbefore\nafter\n4"
          expected = "HEADER\n1\n2\nbefore\nafter\n4"
      addHeader info header sample `shouldBe` expected

    it "adds header after 'putAfter' position" $ do
      let info     = fileInfo $ HeaderConfig ["hs"] ["^before"] "{-|" "-}"
          header   = "HEADER"
          sample   = "1\n2\nbefore\nafter\n4"
          expected = "1\n2\nbefore\nHEADER\nafter\n4"
      addHeader info header sample `shouldBe` expected


  describe "extractFileInfo" $ do
    it "extracts FileInfo from given raw input" $ do
      let config   = HeaderConfig ["hs"] [] "{-|" "-}"
          expected = FileInfo Haskell config (Just (1, 13)) HM.empty
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      extractFileInfo Haskell config sample `shouldBe` expected


  describe "findHeaderPos" $ do
    it "finds single line header" $ do
      let sample = "\n{-| single line -}\n\n"
          config = HeaderConfig ["hs"] [] "{-|" "-}"
      findHeaderPos config sample `shouldBe` Just (1, 1)

    it "finds header put after given regex" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = HeaderConfig ["hs"] ["^foo"] "{-|" "-}"
      findHeaderPos config sample `shouldBe` Just (2, 3)

    it "finds header put after composed regex" $ do
      let sample = "{-| 1 -}\nfoo\n{-| 2\n2 -}\nbar\n{-| 3\n3 -}"
          config = HeaderConfig ["hs"] ["^bar", "^foo"] "{-|" "-}"
      findHeaderPos config sample `shouldBe` Just (5, 6)

    it "finds header in longer example" $ do
      let config = HeaderConfig ["hs"] [] "{-|" "-}"
      sample <- readFileUtf8 $ samplesDir </> "haskell" </> "full.hs"
      findHeaderPos config sample `shouldBe` Just (1, 13)

    it "finds nothing if no header present" $ do
      let sample = "some\nrandom\text without header"
          config = HeaderConfig ["hs"] [] "{-|" "-}"
      findHeaderPos config sample `shouldBe` Nothing

    it "finds nothing if header is present before the 'putAfter' settings" $ do
      let sample = "foo\n{-| 1 -}\nbar\nsome text"
          config = HeaderConfig ["hs"] ["^bar"] "{-|" "-}"
      findHeaderPos config sample `shouldBe` Nothing


  describe "lastMatching" $ do
    let regex = compile "^foo" [utf8]

    it "finds very last line that matches given regex" $ do
      let sample = ["some text", "hello", "foo bar", "last one"]
      lastMatching regex sample `shouldBe` 2

    it "returns 0 if the input is empty" $ do
      lastMatching regex [] `shouldBe` 0


  describe "splitAtHeader" $ do
    it "correctly handles empty input and empty regex" $ do
      --let sample = "random\ntext\nfoo\n{-| 1 -}\nafter"
      splitAtHeader [] "" `shouldBe` (0, [], [])

    it "correctly handles input and empty regex" $ do
      let sample   = "random\ntext\nfoo\n{-| 1 -}\nafter"
          expected = (0, [], ["random", "text", "foo", "{-| 1 -}", "after"])
      splitAtHeader [] sample `shouldBe` expected

    it "correctly splits input for valid regex" $ do
      let sample   = "random\ntext\nfoo\n{-| 1 -}\nafter"
          regexes  = ["^foo"]
          expected = (3, ["random", "text", "foo"], ["{-| 1 -}", "after"])
      splitAtHeader regexes sample `shouldBe` expected

