{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Headroom.Header
Description : License header manipulation
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module is the heart of /Headroom/ as it contains functions for working with
the /license headers/ and the /source code files/.
-}

module Headroom.Header
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

import           Headroom.Configuration.Types   ( CtHeaderConfig
                                                , HeaderConfig(..)
                                                , HeaderSyntax(..)
                                                )
import           Headroom.Data.Lens             ( suffixLensesFor )
import           Headroom.Data.Regex            ( Regex
                                                , match
                                                )
import           Headroom.Data.TextExtra        ( fromLines
                                                , toLines
                                                )
import           Headroom.Ext                   ( extractVariables )
import           Headroom.FileType.Types        ( FileType(..) )
import           Headroom.Header.Types          ( FileInfo(..) )
import           Headroom.Types                 ( TemplateMeta(..) )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


suffixLensesFor ["fiHeaderPos"] ''FileInfo


-- | Extracts info about the processed file to be later used by the header
-- detection/manipulation functions.
extractFileInfo :: FileType
                -- ^ type of the detected file
                -> CtHeaderConfig
                -- ^ license header configuration
                -> Maybe TemplateMeta
                -- ^ metadata extracted from /template/
                -> Text
                -- ^ text used for detection
                -> FileInfo
                -- ^ resulting file info
extractFileInfo fiFileType fiHeaderConfig meta text =
  let fiHeaderPos = findHeader fiHeaderConfig text
      fiVariables =
          extractVariables fiFileType fiHeaderConfig meta fiHeaderPos text
  in  FileInfo { .. }


-- | Adds given header at position specified by the 'FileInfo'. Does nothing if
-- any header is already present, use 'replaceHeader' if you need to
-- override it.
addHeader :: FileInfo
          -- ^ info about file where header is added
          -> Text
          -- ^ text of the new header
          -> Text
          -- ^ text of the file where to add the header
          -> Text
          -- ^ resulting text with added header
addHeader FileInfo {..} _ text | isJust fiHeaderPos = text
addHeader FileInfo {..} header text                 = result
 where
  (before, middle, after) = splitInput hcPutAfter hcPutBefore text
  HeaderConfig {..}       = fiHeaderConfig
  before'                 = stripLinesEnd before
  middle'                 = stripLinesStart middle
  margin [] _      mOuter = replicate mOuter T.empty
  margin _  mInner _      = replicate mInner T.empty
  marginBefore = margin before' hcMarginTopCode hcMarginTopFile
  marginAfter  = margin (middle' <> after) hcMarginBottomCode hcMarginBottomFile
  result       = fromLines $ concat joined
  joined       = [before', marginBefore, [header], marginAfter, middle', after]


-- | Drops header at position specified by the 'FileInfo' from the given text.
-- Does nothing if no header is present.
dropHeader :: FileInfo
           -- ^ info about the file from which the header will be dropped
           -> Text
           -- ^ text of the file from which to drop the header
           -> Text
           -- ^ resulting text with dropped header
dropHeader (FileInfo _ _ Nothing             _) text = text
dropHeader (FileInfo _ _ (Just (start, end)) _) text = result
 where
  before     = take start inputLines
  after      = drop (end + 1) inputLines
  inputLines = toLines text
  result     = fromLines (stripLinesEnd before <> stripLinesStart after)


-- | Replaces existing header at position specified by the 'FileInfo' in the
-- given text. Basically combines 'addHeader' with 'dropHeader'. If no header
-- is present, then the given one is added to the text.
replaceHeader :: FileInfo
              -- ^ info about the file in which to replace the header
              -> Text
              -- ^ text of the new header
              -> Text
              -- ^ text of the file where to replace the header
              -> Text
              -- ^ resulting text with replaced header
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
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> let hc = HeaderConfig ["hs"] 0 0 0 0 [] [] (BlockComment "{-" "-}")
-- >>> findHeader hc "foo\nbar\n{- HEADER -}\nbaz"
-- Just (2,2)
findHeader :: CtHeaderConfig
           -- ^ appropriate header configuration
           -> Text
           -- ^ text in which to detect the header
           -> Maybe (Int, Int)
           -- ^ header position @(startLine, endLine)@
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
findBlockHeader :: Text
                -- ^ starting pattern (e.g. @{-@ or @/*@)
                -> Text
                -- ^ ending pattern (e.g. @-}@ or @*/@)
                -> [Text]
                -- ^ lines of text in which to detect the header
                -> Int
                -- ^ line number offset (adds to resulting position)
                -> Maybe (Int, Int)
                -- ^ header position @(startLine + offset, endLine + offset)@
findBlockHeader startsWith endsWith = go Nothing Nothing 0
 where
  isStart = T.isPrefixOf startsWith
  isEnd   = T.isSuffixOf endsWith
  oneLiner line = isStart line && isEnd line
  ind curr line | isStart line = curr + (1 :: Integer)
  ind curr line | isEnd line   = curr - (1 :: Integer)
  ind curr _                   = curr
  go Nothing _ 0 (x : _) i | oneLiner x    = Just (i, i)
  go Nothing _ 0 (x : xs) i | ind 0 x == 1 = go (Just i) Nothing 1 xs (i + 1)
  go (Just s) _ 1 (x : _) i | ind 1 x == 0 = Just (s, i)
  go s e l (x : xs) i                      = go s e (ind l x) xs (i + 1)
  go _ _ _ []       _                      = Nothing


-- | Finds header in the form of /single-line comment/ syntax, which is
-- delimited with the prefix pattern.
--
-- >>> findLineHeader "--" ["", "a", "-- first", "-- second", "foo"] 0
-- Just (2,3)
findLineHeader :: Text
               -- ^ prefix pattern (e.g. @--@ or @//@)
               -> [Text]
               -- ^ lines of text in which to detect the header
               -> Int
               -- ^ line number offset (adds to resulting position)
               -> Maybe (Int, Int)
               -- ^ header position @(startLine + offset, endLine + offset)@
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
-- >>> import Headroom.Data.Regex (re)
-- >>> :set -XQuasiQuotes
-- >>> firstMatching [[re|^foo|]] ["some text", "foo bar", "foo baz", "last"]
-- Just 1
firstMatching :: [Regex]
              -- ^ /regex/ used for matching
              -> [Text]
              -- ^ input lines
              -> Maybe Int
              -- ^ matching line number
firstMatching patterns input = go input 0
 where
  cond x = any (\r -> isJust $ match r x) patterns
  go (x : _) i | cond x = Just i
  go (_ : xs) i         = go xs (i + 1)
  go []       _         = Nothing


-- | Finds very last line that matches the given /regex/ (numbered from zero).
--
-- >>> import Headroom.Data.Regex (re)
-- >>> :set -XQuasiQuotes
-- >>> lastMatching [[re|^foo|]] ["some text", "foo bar", "foo baz", "last"]
-- Just 2
lastMatching :: [Regex]
             -- ^ /regex/ used for matching
             -> [Text]
             -- ^ input lines
             -> Maybe Int
             -- ^ matching line number
lastMatching patterns input = go input 0 Nothing
 where
  cond x = any (\r -> isJust $ match r x) patterns
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
-- >>> import Headroom.Data.Regex (re)
-- >>> :set -XQuasiQuotes
--
-- >>> splitInput [[re|->|]] [[re|<-|]] "text\n->\nRESULT\n<-\nfoo"
-- (["text","->"],["RESULT"],["<-","foo"])
--
-- >>> splitInput [] [[re|<-|]] "text\n->\nRESULT\n<-\nfoo"
-- ([],["text","->","RESULT"],["<-","foo"])
--
-- >>> splitInput [] [] "one\ntwo"
-- ([],["one","two"],[])
splitInput :: [Regex]
           -- ^ patterns for first split
           -> [Regex]
           -- ^ patterns for second split
           -> Text
           -- ^ text to split
           -> ([Text], [Text], [Text])
           -- ^ result lines as @([before1stSplit], [middle], [after2ndSplit])@
splitInput []       []       input = ([], toLines input, [])
splitInput fstSplit sndSplit input = (before, middle, after)
 where
  (middle', after ) = L.splitAt sndSplitAt inLines
  (before , middle) = L.splitAt fstSplitAt middle'
  fstSplitAt        = maybe 0 (+ 1) (lastMatching fstSplit middle')
  sndSplitAt        = fromMaybe len (firstMatching sndSplit inLines)
  inLines           = toLines input
  len               = L.length inLines


------------------------------  PRIVATE FUNCTIONS  -----------------------------

stripLinesEnd :: [Text] -> [Text]
stripLinesEnd = toLines . T.stripEnd . fromLines


stripLinesStart :: [Text] -> [Text]
stripLinesStart = toLines . T.stripStart . fromLines
