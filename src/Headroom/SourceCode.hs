{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StrictData                 #-}
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
  ( LineType(..)
  , CodeLine
  , SourceCode(..)
  , fromText
  , toText
  , firstMatchingLine
  , lastMatchingLine
  )
where

import           Control.Monad.State                 ( State
                                                     , evalState
                                                     )
import           Data.Coerce                         ( coerce )
import           Headroom.Data.Text                  ( fromLines
                                                     , toLines
                                                     )
import           RIO


data LineType
  = Code
  | Comment
  deriving (Eq, Show)

type CodeLine = (LineType, Text)

newtype SourceCode
  = SourceCode [CodeLine]
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)


fromText :: a -> (Text -> State a LineType) -> Text -> SourceCode
fromText s0 f (toLines -> ls) = coerce $ zip (evalState (mapM f ls) s0) ls


toText :: SourceCode -> Text
toText (SourceCode sc) = fromLines . fmap snd $ sc


firstMatchingLine :: (CodeLine -> Bool) -> SourceCode -> Maybe (Int, CodeLine)
firstMatchingLine f (SourceCode ls) = go ls 0
 where
  go [] _ = Nothing
  go (x : xs) i | f x       = Just (i, x)
                | otherwise = go xs (i + 1)


lastMatchingLine :: (CodeLine -> Bool) -> SourceCode -> Maybe (Int, CodeLine)
lastMatchingLine f (SourceCode ls) =
  let matching = firstMatchingLine f . SourceCode . reverse $ ls
  in  fmap (first ((length ls - 1) -)) matching
