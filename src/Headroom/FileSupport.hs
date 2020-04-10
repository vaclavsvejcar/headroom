{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Headroom.FileSupport
  ( addHeader
  , dropHeader
  , extractFileInfo
  , findHeaderPos
  , lastMatching
  , replaceHeader
  , splitAtHeader
  )
where

import           Control.Lens.TH                ( makeLensesFor )
import           Headroom.Types                 ( FileInfo(..)
                                                , FileType(..)
                                                , HeaderConfig(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
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
  (_, before, after) = splitAtHeader (hcPutAfter fiHeaderConfig) text
  result             = T.intercalate "\n" $ concat [before, [header], after]

dropHeader :: FileInfo -> Text -> Text
dropHeader (FileInfo _ _ Nothing             _) text = text
dropHeader (FileInfo _ _ (Just (start, end)) _) text = result
 where
  before     = take start inputLines
  after      = drop (end + 1) inputLines
  inputLines = T.lines text
  result     = T.intercalate "\n" (before ++ after)

extractFileInfo :: FileType -> HeaderConfig -> Text -> FileInfo
extractFileInfo fiFileType fiHeaderConfig input =
  let fiHeaderPos = findHeaderPos fiHeaderConfig input
      fiVariables = extractVariables fiFileType fiHeaderConfig input
  in  FileInfo { .. }

-- TODO: https://github.com/vaclavsvejcar/headroom/issues/25
extractVariables :: FileType -> HeaderConfig -> Text -> HashMap Text Text
extractVariables _ _ _ = HM.empty

findHeaderPos :: HeaderConfig -> Text -> Maybe (Int, Int)
findHeaderPos HeaderConfig {..} input = position
 where
  (splitAt, _, headerArea) = splitAtHeader hcPutAfter input
  isStart                  = T.isPrefixOf hcStartsWith
  isEnd                    = T.isSuffixOf hcEndsWith
  position                 = go (T.strip <$> headerArea) Nothing Nothing splitAt
   where
    go (x : _) _ _ i | isStart x && isEnd x = Just (i, i)
    go (x : xs) _ _ i | isStart x           = go xs (Just i) Nothing (i + 1)
    go (x : _) (Just start) _ i | isEnd x   = Just (start, i)
    go (_ : xs) start end i                 = go xs start end (i + 1)
    go []       _     _   _                 = Nothing

lastMatching :: Regex -> [Text] -> Int
lastMatching regex input = go input 0 0
 where
  go (x : xs) _ i | isJust $ match regex (encodeUtf8 x) [] = go xs i (i + 1)
  go (_ : xs) pos i = go xs pos (i + 1)
  go []       pos _ = pos

replaceHeader :: FileInfo -> Text -> Text -> Text
replaceHeader fileInfo header = addOp . dropOp
 where
  addOp          = addHeader infoWithoutPos header
  dropOp         = dropHeader fileInfo
  infoWithoutPos = set fiHeaderPosL Nothing fileInfo

splitAtHeader :: [Text] -> Text -> (Int, [Text], [Text])
splitAtHeader []       input = (0, [], T.lines input)
splitAtHeader putAfter input = (splitAt, before, after)
 where
  before  = take splitAt inLines
  after   = drop splitAt inLines
  inLines = T.lines input
  regex   = compile (encodeUtf8 $ T.intercalate "|" putAfter) [utf8]
  splitAt = lastMatching regex inLines + 1
