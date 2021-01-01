{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Headroom.Header.Sanitize
Description : Logic for sanitizing license headers
Copyright   : (c) 2019-2021 Vaclav Svejcar
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
  , sanitizeSyntax
  )
where

import           Headroom.Configuration.Types        ( HeaderSyntax(..) )
import           Headroom.Data.Regex                 ( isMatch )
import           Headroom.Data.TextExtra             ( commonLinesPrefix
                                                     , fromLines
                                                     , mapLinesF
                                                     , toLines
                                                     )
import           RIO
import qualified RIO.Text                           as T


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Tries to find comment prefix in given comment. By /prefix/ it's meant
-- either the line prefix used for block comment syntax (like @*@ at start of
-- each line between opening and closing pattern - @/* */@) or line comment
-- syntax (just the syntax for comment itself - like @//@ or @--@). If such
-- prefix is found, it's then added to the input 'HeaderSyntax'.
--
-- >>> :set -XQuasiQuotes
-- >>> import Headroom.Data.Regex (re)
-- >>> findPrefix (BlockComment [re|^\/\*|] [re|\*\/$|] Nothing) "/*\n * foo\n * bar\n */"
-- BlockComment "^\\/\\*" "\\*\\/$" (Just " *")
findPrefix :: HeaderSyntax
           -- ^ describes comment syntax of the header
           -> Text
           -- ^ text containint the comment
           -> HeaderSyntax
           -- ^ input 'HeaderSyntax' with added prefix (if found)
findPrefix syntax text = case syntax of
  BlockComment s e _ -> BlockComment s e prefix
  LineComment s _    -> LineComment s prefix
 where
  filtered = filter cond . toLines $ text
  cond     = \t -> (not . T.null . T.strip $ t) && isCommentBody syntax t
  prefix   = fmap T.stripEnd (commonLinesPrefix . fromLines $ filtered)


-- | Sanitizes given header text to make sure that each comment line starts with
-- appropriate prefix (if defined within given 'HeaderSyntax'). For block
-- comments, this is to make it visually unified, but for line comments it's
-- necessary in order not to break syntax of target source code file.
--
-- >>> :set -XQuasiQuotes
-- >>> import Headroom.Data.Regex (re)
-- >>> sanitizeSyntax (LineComment [re|^--|] (Just "--")) "-- foo\nbar"
-- "-- foo\n-- bar"
sanitizeSyntax :: HeaderSyntax
               -- ^ header syntax definition that may contain prefix
               -> Text
               -- ^ header to sanitize
               -> Text
               -- ^ sanitized header
sanitizeSyntax syntax = mapCommentLines syntax (addPrefix mPrefix)
 where
  addPrefix Nothing l = Just l
  addPrefix (Just p) l | p `T.isPrefixOf` l = Just l
                       | otherwise          = Just $ p <> " " <> l
  mPrefix = case syntax of
    BlockComment _ _ p -> p
    LineComment _ p    -> p


------------------------------  PRIVATE FUNCTIONS  -----------------------------

mapCommentLines :: Foldable t
                => HeaderSyntax
                -> (Text -> t Text)
                -> Text
                -> Text
mapCommentLines syntax f = mapLinesF $ \case
  line | isCommentBody syntax line -> toList . f $ line
       | otherwise                 -> [line]


isCommentBody :: HeaderSyntax -> Text -> Bool
isCommentBody (LineComment _ _   ) _ = True
isCommentBody (BlockComment s e _) l = not $ isMatch s l || isMatch e l
