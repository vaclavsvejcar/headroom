{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Headroom.FileSupport
  ( addHeader
  , dropHeader
  , replaceHeader
  , extractFileInfo
  , findHeader
  , findBlockHeader
  , findPrefixedHeader
  , firstMatching
  , lastMatching
  , splitInput
  )
where

import           Control.Lens.TH                ( makeLensesFor )
import           Headroom.Types                 ( FileInfo(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Light          ( Regex
                                                , compile
                                                , match
                                                )
import           Text.Regex.PCRE.Light.Char8    ( utf8 )


makeLensesFor [("fiHeaderPos", "fiHeaderPosL")] ''FileInfo

addHeader :: FileInfo -> Text -> Text -> Text
addHeader FileInfo {..} _ text | isJust fiHeaderPos = text
addHeader FileInfo {..} header text                 = result
 where
  (before, middle, after) = splitInput hcPutAfter hcPutBefore text
  result                  = T.unlines $ concat joined
  joined                  = [before, [header], middle, after]
  HeaderConfig {..}       = fiHeaderConfig


dropHeader :: FileInfo -> Text -> Text
dropHeader (FileInfo _ _ Nothing             _) text = text
dropHeader (FileInfo _ _ (Just (start, end)) _) text = result
 where
  before     = take start inputLines
  after      = drop (end + 1) inputLines
  inputLines = T.lines text
  result     = T.unlines (before ++ after)


replaceHeader :: FileInfo -> Text -> Text -> Text
replaceHeader fileInfo header = addOp . dropOp
 where
  addOp          = addHeader infoWithoutPos header
  dropOp         = dropHeader fileInfo
  infoWithoutPos = set fiHeaderPosL Nothing fileInfo


extractFileInfo :: FileType -> HeaderConfig -> Text -> FileInfo
extractFileInfo fiFileType fiHeaderConfig input =
  let fiHeaderPos = findHeader fiHeaderConfig input
      fiVariables = extractVariables fiFileType fiHeaderConfig input
  in  FileInfo { .. }


-- TODO: https://github.com/vaclavsvejcar/headroom/issues/25
extractVariables :: FileType -> HeaderConfig -> Text -> HashMap Text Text
extractVariables _ _ _ = HM.empty

findHeader :: HeaderConfig -> Text -> Maybe (Int, Int)
findHeader HeaderConfig {..} input = case hcHeaderSyntax of
  PrefixedHeader prefix -> findPrefixedHeader prefix inLines splitAt
  BlockHeader start end -> findBlockHeader start end inLines splitAt
 where
  (before, headerArea, _) = splitInput hcPutAfter hcPutBefore input
  splitAt                 = L.length before
  inLines                 = T.strip <$> headerArea


findBlockHeader :: Text -> Text -> [Text] -> Int -> Maybe (Int, Int)
findBlockHeader startsWith endsWith = go Nothing Nothing
 where
  isStart = T.isPrefixOf startsWith
  isEnd   = T.isSuffixOf endsWith
  go _ _ (x : _) i | isStart x && isEnd x = Just (i, i)
  go _ _ (x : xs) i | isStart x           = go (Just i) Nothing xs (i + 1)
  go (Just start) _ (x : _) i | isEnd x   = Just (start, i)
  go start end (_ : xs) i                 = go start end xs (i + 1)
  go _     _   []       _                 = Nothing


findPrefixedHeader :: Text -> [Text] -> Int -> Maybe (Int, Int)
findPrefixedHeader prefix = go Nothing
 where
  isPrefix = T.isPrefixOf prefix
  go Nothing (x : xs) i | isPrefix x      = go (Just i) xs (i + 1)
  go Nothing (_ : xs) i                   = go Nothing xs (i + 1)
  go (Just start) (x : xs) i | isPrefix x = go (Just start) xs (i + 1)
  go (Just start) _  i                    = Just (start, i - 1)
  go _            [] _                    = Nothing


firstMatching :: Regex -> [Text] -> Int
firstMatching regex input = go input 0
 where
  go (x : _) i | isJust $ match regex (encodeUtf8 x) [] = i
  go (_ : xs) i = go xs (i + 1)
  go []       i = i


lastMatching :: Regex -> [Text] -> Int
lastMatching regex input = go input 0 0
 where
  go (x : xs) _ i | isJust $ match regex (encodeUtf8 x) [] = go xs i (i + 1)
  go (_ : xs) pos i = go xs pos (i + 1)
  go []       pos _ = pos


splitInput :: [Text] -> [Text] -> Text -> ([Text], [Text], [Text])
splitInput []       []        input = ([], T.lines input, [])
splitInput putAfter putBefore input = (before, middle, after)
 where
  before  = take fstSplitAt inLines
  middle  = drop fstSplitAt . take sndSplitAt $ inLines
  after   = drop sndSplitAt inLines
  inLines = T.lines input
  fstSplitAt | null putAfter = 0
             | otherwise     = lastMatching (compile' putAfter) inLines + 1
  sndSplitAt | null putBefore = L.length inLines
             | otherwise      = firstMatching (compile' putBefore) inLines
  compile' regex = compile (encodeUtf8 $ T.intercalate "|" regex) [utf8]
