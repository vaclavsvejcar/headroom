{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.Header.Sanitize
Description : Logic for sanitizing license headers
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions related to sanitizing license headers. Because
license headers are just regular comments in given programming language, they
need to have correct syntax in order to avoid causing compile/runtime errors.
Because header manipulation done by /Headroom/ can disrupt the comment syntax
structure, sanitizing the header is the last step done in the flow, making
sure that license header syntax is not broken.
-}

module Headroom.Header.Sanitize
  ( findPrefix
  )
where

import           Headroom.Configuration.Types        ( HeaderSyntax(..) )
import           Headroom.Data.TextExtra             ( commonLinesPrefix
                                                     , fromLines
                                                     , toLines
                                                     )
import           RIO
import           RIO.List.Partial                    ( init
                                                     , tail
                                                     )
import qualified RIO.Text                           as T


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Tries to find comment prefix in given comment. By /prefix/ it's meant
-- either the line prefix used for block comment syntax (like @*@ at start of
-- each line between opening and closing pattern - @/* */@) or line comment
-- syntax (just the syntax for comment itself - like @//@ or @--@). If such
-- prefix is found, it's then added to the input 'HeaderSyntax'.
--
-- >>> findPrefix (BlockComment "/*" "*/" Nothing) "/*\n * foo\n * bar\n */"
-- BlockComment "/*" "*/" (Just " *")
findPrefix :: HeaderSyntax
           -- ^ describes comment syntax of the header
           -> Text
           -- ^ text containint the comment
           -> HeaderSyntax
           -- ^ input 'HeaderSyntax' with added prefix (if found)
findPrefix syntax text = case syntax of
  LineComment s _    -> LineComment s (linePrefix filtered)
  BlockComment s e p -> BlockComment s e (blockPrefix filtered p)
 where
  filtered = filter (not . T.null . T.strip) . toLines $ text
  prefix t = fmap T.stripEnd (commonLinesPrefix . fromLines $ t)
  linePrefix = prefix
  blockPrefix xs@(_ : _) _ = prefix $ tail (init xs)
  blockPrefix []         p = p
