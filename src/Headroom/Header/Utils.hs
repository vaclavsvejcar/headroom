{-|
Module      : Headroom.Header.Utils
Description : License header utilities
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Useful functions for searching specific text fragments in input text, used by
other modules to detect existing license headers in source code files.
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
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light

-- | Finds line in given text that matches given predicate and returns its line
-- number.
findLine :: (Text -> Bool) -- ^ predicate to find line
         -> Text           -- ^ input text
         -> Int            -- ^ number of line that matches given predicate
findLine predicate text =
  fromMaybe 0 $ L.findIndex (predicate . T.strip) (T.lines text)

-- | Finds line starting with one of given patterns and returns its line number
-- (specialized form of 'findLine').
findLineStartingWith :: [Text] -- ^ patterns to use
                     -> Text   -- ^ input text
                     -> Int    -- ^ number of line starting with one of patterns
findLineStartingWith patterns = findLine predicate
  where predicate line = or $ fmap (`T.isPrefixOf` line) patterns

-- | Count lines that matches the given (multiline) regex. Useful for example to
-- find how many lines are taken by multi-line comment in source code.
linesCountByRegex :: Regex -- ^ regular expression to use
                  -> Text  -- ^ input text
                  -> Int   -- ^ number of lines matching given regex
linesCountByRegex regex text = case L.headMaybe $ scan regex text of
  Just (comment, _) -> L.length . T.lines $ comment
  _                 -> 0

-- | Regex configuration for matching multi-line UTF strings.
reML :: QuasiQuoter
reML = mkRegexQQ [dotall, utf8]
