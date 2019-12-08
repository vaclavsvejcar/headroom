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
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T

findLine :: (T.Text -> Bool) -> T.Text -> Int
findLine predicate text =
  fromMaybe 0 $ L.findIndex (predicate . T.strip) (T.lines text)

findLineStartingWith :: [T.Text] -> T.Text -> Int
findLineStartingWith patterns = findLine predicate
  where predicate line = or $ fmap (`T.isPrefixOf` line) patterns
