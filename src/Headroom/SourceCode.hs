{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.SourceCode
-- Description : Type safe representation of analyzed source code
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types and function used for analysis and type safe
-- representation of source code files.
module Headroom.SourceCode (
    -- * Data Types
      LineType (..)
    , CodeLine
    , SourceCode (..)

      -- * Functions
    , fromText
    , toText
    , firstMatching
    , lastMatching
    , stripStart
    , stripEnd
    , cut
) where

import Control.Monad.State (
    State
    , evalState
 )
import Headroom.Data.Coerce (
    coerce
    , inner
 )
import Headroom.Data.Text (
    fromLines
    , toLines
 )
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

---------------------------------  DATA TYPES  ---------------------------------

-- | Represents type of the line in source code.
data LineType
    = -- | Line of code
      Code
    | -- | Line of comment
      Comment
    deriving (Eq, Show)

-- | Type alias for analyzed line of code.
type CodeLine = (LineType, Text)

-- | Represents analyzed source code.
newtype SourceCode
    = SourceCode [CodeLine]
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Converts 'Text' into 'SourceCode' using the given function to analyze
-- each line's 'LineType'. The analyzing function can hold any state that is
-- accumulated as the text is processed, for example to hold some info about
-- already processed lines.
fromText ::
    -- | initial state of analyzing function
    a ->
    -- | function that analyzes currently processed line
    (Text -> State a LineType) ->
    -- | raw source code to analyze
    Text ->
    -- | analyzed 'SourceCode'
    SourceCode
fromText s0 f (toLines -> ls) = coerce $ zip (evalState (mapM f ls) s0) ls

-- | Converts analyzed 'SourceCode' back into 'Text'.
toText ::
    -- | source code to convert back to plain text
    SourceCode ->
    -- | resulting plain text
    Text
toText (SourceCode sc) = fromLines . fmap snd $ sc

-- | Finds very first line matching given predicate and optionally performs some
-- operation over it.
firstMatching ::
    -- | predicate (and transform) function
    (CodeLine -> Maybe a) ->
    -- | source code to search in
    SourceCode ->
    -- | first matching line (if found)
    Maybe (Int, a)
firstMatching f sc = go (coerce sc) 0
  where
    go [] _ = Nothing
    go (x : xs) i
        | Just res <- f x = Just (i, res)
        | otherwise = go xs (i + 1)

-- | Finds very last line matching given predicate and optionally performs some
-- operation over it.
lastMatching ::
    -- | predicate (and transform) function
    (CodeLine -> Maybe a) ->
    -- | source code to search in
    SourceCode ->
    -- | last matching line (if found)
    Maybe (Int, a)
lastMatching f sc =
    let matching = firstMatching f . inner @_ @[CodeLine] reverse $ sc
        lastIdx = length (coerce sc :: [CodeLine]) - 1
     in fmap (first (lastIdx -)) matching

-- | Strips empty lines at the beginning of source code.
--
-- >>> stripStart $ SourceCode [(Code, ""), (Code, "foo"), (Code, "")]
-- SourceCode [(Code,"foo"),(Code,"")]
stripStart ::
    -- | source code to strip
    SourceCode ->
    -- | stripped source code
    SourceCode
stripStart = inner @_ @[CodeLine] (L.dropWhile (T.null . T.strip . snd))

-- | Strips empty lines at the end of source code.
--
-- >>> stripEnd $ SourceCode [(Code, ""), (Code, "foo"), (Code, "")]
-- SourceCode [(Code,""),(Code,"foo")]
stripEnd ::
    -- | source code to strip
    SourceCode ->
    -- | stripped source code
    SourceCode
stripEnd = inner @_ @[CodeLine] (L.dropWhileEnd (T.null . T.strip . snd))

-- | Cuts snippet from the source code using the given start and end position.
--
-- >>> cut 1 3 $ SourceCode [(Code, "1"), (Code, "2"),(Code, "3"),(Code, "4")]
-- SourceCode [(Code,"2"),(Code,"3")]
cut ::
    -- | index of first line to be included into the snippet
    Int ->
    -- | index of the first line after the snippet
    Int ->
    -- | source code to cut
    SourceCode ->
    -- | cut snippet
    SourceCode
cut s e = inner @_ @[CodeLine] (L.take (e - s) . L.drop s)
