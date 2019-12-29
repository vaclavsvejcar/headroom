{-|
Module      : Headroom.Header.Utils
Description : License header utilities
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Shared data types and functions for license header functionality.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header.Utils
  ( findLine
  , findLineStartingWith
  , linesCountByRegex
  , reML
  )
where

import           Language.Haskell.TH.Quote      ( QuasiQuoter )
import           RIO
import qualified RIO.List                      as L
import           RIO.Text                       ( Text )
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light

findLine :: (Text -> Bool) -> Text -> Int
findLine predicate text =
  fromMaybe 0 $ L.findIndex (predicate . T.strip) (T.lines text)

findLineStartingWith :: [Text] -> Text -> Int
findLineStartingWith patterns = findLine predicate
  where predicate line = or $ fmap (`T.isPrefixOf` line) patterns

linesCountByRegex :: Regex -> Text -> Int
linesCountByRegex regex text = case L.headMaybe $ scan regex text of
  Just (comment, _) -> L.length . T.lines $ comment
  _                 -> 0

-- | Regex configuration for matching multi-line UTF strings.
reML :: QuasiQuoter
reML = mkRegexQQ [dotall, utf8]
