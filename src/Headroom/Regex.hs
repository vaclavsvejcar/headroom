{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Headroom.Regex
Description : Helper functions for regular expressions
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Extends functionalify provided by "Text.Regex.PCRE.Light"
and "Text.Regex.PCRE.Heavy" that more suits the needs of this application.
-}

module Headroom.Regex
  ( compile'
  , joinPatterns
  , gsub
  , match'
  , re'
  , regexOptions
  , scan
  , sub
  )
where

import           Language.Haskell.TH.Quote      ( QuasiQuoter )
import           RIO
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Heavy          ( gsub
                                                , mkRegexQQ
                                                , scan
                                                , sub
                                                )
import           Text.Regex.PCRE.Light          ( PCREOption
                                                , Regex
                                                , compile
                                                )
import           Text.Regex.PCRE.Light.Char8    ( match
                                                , utf8
                                                )


-- | Same as 'compile', but takes 'Text' on input and enables 'utf8' option
-- by default.
compile' :: Text
         -- ^ regular expression to be compiled
         -> Regex
         -- ^ compiled regular expression
compile' regex = compile (encodeUtf8 regex) regexOptions


-- | Joins list of patterns into single regex string. If the input list is
-- empty, 'Nothing' is returned.
--
-- >>> joinPatterns ["^foo", "^bar"]
-- Just "^foo|^bar"
joinPatterns :: [Text]
             -- ^ list of patterns to join
             -> Maybe Text
             -- ^ joined patterns
joinPatterns [] = Nothing
joinPatterns ps = Just $ T.intercalate "|" ps


-- | Same as 'match', but works with 'Text' and uses no additional options.
match' :: Regex
       -- ^ a PCRE regular expression value produced by compile
       -> Text
       -- ^ the subject text to match against
       -> Maybe [Text]
       -- ^ the result value
match' regex subject = fmap T.pack <$> match regex (T.unpack subject) []


-- | /QuasiQuoter/ for "Text.Regex.PCRE.Heavy" compile-time regular expressions.
re' :: QuasiQuoter
re' = mkRegexQQ regexOptions


-- | /PCRE/ options for regular expressions.
regexOptions :: [PCREOption]
regexOptions = [utf8]
