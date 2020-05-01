{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Headroom.FileSupport
Description : License header manipulation
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module is the heart of /Headroom/ as it contains functions for working with
the /license headers/ and the /source code files/.
-}

module Headroom.FileSupport
  ( -- * File info extraction
    extractFileInfo
    -- * License header manipulation
  , addHeader
  , dropHeader
  , replaceHeader
    -- * License header detection
  , findHeader
  , findBlockHeader
  , findLineHeader
  , firstMatching
  , lastMatching
  , splitInput
  )
where

import           Headroom.Regex                 ( compile'
                                                , joinPatterns
                                                , match'
                                                )
import           Headroom.Types                 ( FileInfo(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Light          ( Regex )



-- | Extracts info about the processed file to be later used by the header
-- detection/manipulation functions.
extractFileInfo :: FileType     -- ^ type of the detected file
                -> HeaderConfig -- ^ appropriate header configuration
                -> Text         -- ^ text used for detection
                -> FileInfo     -- ^ resulting file info
extractFileInfo fiFileType fiHeaderConfig input =
  let fiHeaderPos = findHeader fiHeaderConfig input
      fiVariables = extractVariables fiFileType fiHeaderConfig input
  in  FileInfo { .. }


-- | Adds given header at position specified by the 'FileInfo'. Does nothing if
-- any header is already present, use 'replaceHeader' if you need to
-- override it.
addHeader :: FileInfo -- ^ info about file where header is added
          -> Text     -- ^ text of the new header
          -> Text     -- ^ text of the file where to add the header
          -> Text     -- ^ resulting text with added header
addHeader FileInfo {..} _ text | isJust fiHeaderPos = text
addHeader FileInfo {..} header text                 = result
 where
  (before, middle, after) = splitInput hcPutAfter hcPutBefore text
  HeaderConfig {..}       = fiHeaderConfig
  before'                 = stripLinesEnd before
  middle'                 = stripLinesStart middle
  margin [] _    = []
  margin _  size = replicate size ""
  marginBefore = margin before' hcMarginBefore
  marginAfter  = margin (middle' <> after) hcMarginAfter
  result       = T.unlines $ concat joined
  joined       = [before', marginBefore, [header], marginAfter, middle', after]


-- | Drops header at position specified by the 'FileInfo' from the given text.
-- Does nothing if no header is present.
dropHeader :: FileInfo -- ^ info about the file from which the header will be dropped
           -> Text     -- ^ text of the file from which to drop the header
           -> Text     -- ^ resulting text with dropped header
dropHeader (FileInfo _ _ Nothing             _) text = text
dropHeader (FileInfo _ _ (Just (start, end)) _) text = result
 where
  before     = take start inputLines
  after      = drop (end + 1) inputLines
  inputLines = T.lines text
  result     = T.unlines (stripLinesEnd before ++ stripLinesStart after)


-- | Replaces existing header at position specified by the 'FileInfo' in the
-- given text. Basically combines 'addHeader' with 'dropHeader'. If no header
-- is present, then the given one is added to the text.
replaceHeader :: FileInfo -- ^ info about the file in which to replace the header
              -> Text     -- ^ text of the new header
              -> Text     -- ^ text of the file where to replace the header
              -> Text     -- ^ resulting text with replaced header
replaceHeader fileInfo header = addHeader' . dropHeader'
 where
  addHeader'     = addHeader infoWithoutPos header
  dropHeader'    = dropHeader fileInfo
  infoWithoutPos = set fiHeaderPosL Nothing fileInfo


-- | Finds header position in given text, where position is represented by
-- line number of first and last line of the header (numbered from zero).
-- Based on the 'HeaderSyntax' specified in given 'HeaderConfig', this function
-- delegates its work to either 'findBlockHeader' or 'findLineHeader'.
--
-- >>> let hc = HeaderConfig ["hs"] 0 0 [] [] (BlockComment "{-" "-}")
-- >>> findHeader hc "foo\nbar\n{- HEADER -}\nbaz"
-- Just (2,2)
findHeader :: HeaderConfig     -- ^ appropriate header configuration
           -> Text             -- ^ text in which to detect the header
           -> Maybe (Int, Int) -- ^ header position @(startLine, endLine)@
findHeader HeaderConfig {..} input = case hcHeaderSyntax of
  BlockComment start end -> findBlockHeader start end inLines splitAt
  LineComment prefix     -> findLineHeader prefix inLines splitAt
 where
  (before, headerArea, _) = splitInput hcPutAfter hcPutBefore input
  splitAt                 = L.length before
  inLines                 = T.strip <$> headerArea


-- | Finds header in the form of /multi-line comment/ syntax, which is delimited
-- with starting and ending pattern.
--
-- >>> findBlockHeader "{-" "-}" ["", "{- HEADER -}", "", ""] 0
-- Just (1,1)
findBlockHeader :: Text             -- ^ starting pattern (e.g. @{-@ or @/*@)
                -> Text             -- ^ ending pattern (e.g. @-}@ or @*/@)
                -> [Text]           -- ^ lines of text in which to detect the header
                -> Int              -- ^ line number offset (adds to resulting position)
                -> Maybe (Int, Int) -- ^ header position @(startLine + offset, endLine + offset)@
findBlockHeader startsWith endsWith = go Nothing Nothing
 where
  isStart = T.isPrefixOf startsWith
  isEnd   = T.isSuffixOf endsWith
  go _ _ (x : _) i | isStart x && isEnd x = Just (i, i)
  go _ _ (x : xs) i | isStart x           = go (Just i) Nothing xs (i + 1)
  go (Just start) _ (x : _) i | isEnd x   = Just (start, i)
  go start end (_ : xs) i                 = go start end xs (i + 1)
  go _     _   []       _                 = Nothing


-- | Finds header in the form of /single-line comment/ syntax, which is
-- delimited with the prefix pattern.
--
-- >>> findLineHeader "--" ["", "a", "-- first", "-- second", "foo"] 0
-- Just (2,3)
findLineHeader :: Text             -- ^ prefix pattern (e.g. @--@ or @//@)
               -> [Text]           -- ^ lines of text in which to detect the header
               -> Int              -- ^ line number offset (adds to resulting position)
               -> Maybe (Int, Int) -- ^ header position @(startLine + offset, endLine + offset)@
findLineHeader prefix = go Nothing
 where
  isPrefix = T.isPrefixOf prefix
  go Nothing (x : xs) i | isPrefix x      = go (Just i) xs (i + 1)
  go Nothing (_ : xs) i                   = go Nothing xs (i + 1)
  go (Just start) (x : xs) i | isPrefix x = go (Just start) xs (i + 1)
  go (Just start) _  i                    = Just (start, i - 1)
  go _            [] _                    = Nothing


-- | Finds very first line that matches the given /regex/ (numbered from zero).
--
-- >>> firstMatching (compile' "^foo") ["some text", "foo bar", "foo baz", "last"]
-- Just 1
firstMatching :: Regex        -- /regex/ used for matching
              -> [Text]       -- input lines
              -> Maybe Int    -- matching line number
firstMatching regex input = go input 0
 where
  cond x = isJust $ match' regex x
  go (x : _) i | cond x = Just i
  go (_ : xs) i         = go xs (i + 1)
  go []       _         = Nothing


-- | Finds very last line that matches the given /regex/ (numbered from zero).
--
-- >>> lastMatching (compile' "^foo") ["some text", "foo bar", "foo baz", "last"]
-- Just 2
lastMatching :: Regex        -- /regex/ used for matching
             -> [Text]       -- input lines
             -> Maybe Int    -- matching line number
lastMatching regex input = go input 0 Nothing
 where
  cond x = isJust $ match' regex x
  go (x : xs) i _ | cond x = go xs (i + 1) (Just i)
  go (_ : xs) i pos        = go xs (i + 1) pos
  go []       _ pos        = pos


-- | Splits input lines into three parts:
--
--     1. list of all lines located before the very last occurence of one of
--        the conditions from the first condition list
--     2. list of all lines between the first and last lists
--     3. list of all lines located after the very first occurence of one of
--        the conditions from the second condition list
--
-- If both first and second patterns are empty, then all lines are returned in
-- the middle list.
--
-- >>> splitInput ["->"] ["<-"] "text\n->\nRESULT\n<-\nfoo"
-- (["text","->"],["RESULT"],["<-","foo"])
--
-- >>> splitInput [] ["<-"] "text\n->\nRESULT\n<-\nfoo"
-- ([],["text","->","RESULT"],["<-","foo"])
--
-- >>> splitInput [] [] "one\ntwo"
-- ([],["one","two"],[])
splitInput :: [Text] -> [Text] -> Text -> ([Text], [Text], [Text])
splitInput []       []       input = ([], T.lines input, [])
splitInput fstSplit sndSplit input = (before, middle, after)
 where
  (middle', after ) = L.splitAt sndSplitAt inLines
  (before , middle) = L.splitAt fstSplitAt middle'
  fstSplitAt        = maybe 0 (+ 1) (findSplit lastMatching fstSplit middle')
  sndSplitAt        = fromMaybe len (findSplit firstMatching sndSplit inLines)
  inLines           = T.lines input
  len               = L.length inLines
  findSplit f ps i = compile' <$> joinPatterns ps >>= (`f` i)


-- TODO: https://github.com/vaclavsvejcar/headroom/issues/25
extractVariables :: FileType -> HeaderConfig -> Text -> HashMap Text Text
extractVariables _ _ _ = HM.empty


stripLinesEnd :: [Text] -> [Text]
stripLinesEnd = takeWhile (not . T.null . T.strip)


stripLinesStart :: [Text] -> [Text]
stripLinesStart = dropWhile (T.null . T.strip)

fiHeaderPosL :: Lens' FileInfo (Maybe (Int, Int))
fiHeaderPosL = lens fiHeaderPos (\x y -> x { fiHeaderPos = y })
