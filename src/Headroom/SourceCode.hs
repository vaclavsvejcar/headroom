{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

{-|
Module      : Headroom.SourceCode
Description : Type safe representation of analyzed source code
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types and function used for analysis and type safe
representation of source code files.
-}

module Headroom.SourceCode
  ( -- * Data Types
    LineType(..)
  , CodeLine
  , SourceCode(..)
    -- * Functions
  , fromText
  , toText
  , firstMatching
  , lastMatching
  , stripStart
  , stripEnd
  , cut
  )
where

import           Control.Monad.State                 ( State
                                                     , evalState
                                                     )
import           Headroom.Data.Coerce                ( coerce
                                                     , inner
                                                     )
import           Headroom.Data.Text                  ( fromLines
                                                     , toLines
                                                     )
import           RIO
import qualified RIO.List                           as L
import qualified RIO.Text                           as T


---------------------------------  DATA TYPES  ---------------------------------

-- | Represents type of the line in source code.
data LineType
  = Code    -- ^ Line of code
  | Comment -- ^ Line of comment
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
fromText :: a
         -- ^ initial state of analyzing function
         -> (Text -> State a LineType)
         -- ^ function that analyzes currently processed line
         -> Text
         -- ^ raw source code to analyze
         -> SourceCode
         -- ^ analyzed 'SourceCode'
fromText s0 f (toLines -> ls) = coerce $ zip (evalState (mapM f ls) s0) ls


-- | Converts analyzed 'SourceCode' back into 'Text'.
toText :: SourceCode
       -- ^ source code to convert back to plain text
       -> Text
       -- ^ resulting plain text
toText (SourceCode sc) = fromLines . fmap snd $ sc


-- | Finds very first line that matches the given predicate.
firstMatching :: (CodeLine -> Bool)
              -- ^ predicate function
              -> SourceCode
              -- ^ source code to search in
              -> Maybe (Int, CodeLine)
              -- ^ first matching line (if found)
firstMatching f (SourceCode ls) = go ls 0
 where
  go [] _ = Nothing
  go (x : xs) i | f x       = Just (i, x)
                | otherwise = go xs (i + 1)


-- | Finds very last line that matches the given predicate.
lastMatching :: (CodeLine -> Bool)
             -- ^ predicate function
             -> SourceCode
             -- ^ source code to search in
             -> Maybe (Int, CodeLine)
             -- ^ last matching line (if found)
lastMatching f (SourceCode ls) =
  let matching = firstMatching f . SourceCode . reverse $ ls
  in  fmap (first ((length ls - 1) -)) matching


-- | Strips empty lines at the beginning of source code.
--
-- >>> stripStart $ SourceCode [(Code, ""), (Code, "foo"), (Code, "")]
-- SourceCode [(Code,"foo"),(Code,"")]
stripStart :: SourceCode
           -- ^ source code to strip
           -> SourceCode
           -- ^ stripped source code
stripStart = inner @_ @[CodeLine] (L.dropWhile (T.null . T.strip . snd))


-- | Strips empty lines at the end of source code.
--
-- >>> stripEnd $ SourceCode [(Code, ""), (Code, "foo"), (Code, "")]
-- SourceCode [(Code,""),(Code,"foo")]
stripEnd :: SourceCode
         -- ^ source code to strip
         -> SourceCode
         -- ^ stripped source code
stripEnd = inner @_ @[CodeLine] (L.dropWhileEnd (T.null . T.strip . snd))


-- | Cuts snippet from the source code using the given start and end position.
--
-- >>> cut 1 3 $ SourceCode [(Code, "1"), (Code, "2"),(Code, "3"),(Code, "4")]
-- SourceCode [(Code,"2"),(Code,"3")]
cut :: Int
    -- ^ index of first line to be included into the snippet
    -> Int
    -- ^ index of the first line after the snippet
    -> SourceCode
    -- ^ source code to cut
    -> SourceCode
    -- ^ cut snippet
cut s e = inner @_ @[CodeLine] (L.take (e - s) . L.drop s)
