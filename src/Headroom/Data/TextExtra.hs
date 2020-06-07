{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.Data.TextExtra
Description : Additional utilities for text manipulation
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module containing bunch of useful functions for working with text.
-}

module Headroom.Data.TextExtra
  ( mapLines
  , read
  )
where

import           RIO
import qualified RIO.Text                      as T



-- | Maps given function over individual lines of the given text.
--
-- >>> mapLines ("T: " <>) "foo zz\nbar\n"
-- "T: foo zz\nT: bar\n"
mapLines :: (Text -> Text)
         -- ^ function to map over individual lines
         -> Text
         -- ^ input text
         -> Text
         -- ^ result text
mapLines fn = T.unlines . go . T.lines
 where
  go []       = []
  go (x : xs) = fn x : go xs


-- | Same as 'readMaybe', but takes 'Text' as input instead of 'String'.
--
-- >>> read "123" :: Maybe Int
-- Just 123
read :: Read a
     => Text
     -- ^ input text to parse
     -> Maybe a
     -- ^ parsed value
read = readMaybe . T.unpack
