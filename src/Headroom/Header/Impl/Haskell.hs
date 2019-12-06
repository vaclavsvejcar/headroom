{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Haskell
  ( headerSizeHaskell
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


headerSizeHaskell :: T.Text -> Int
headerSizeHaskell = findLine
 where
  patterns = ["{-#", "module"]
  findLine text = findIndex lineStartsWith (T.lines text)
  findIndex cond xs = fromMaybe 0 (L.findIndex cond xs)
  lineStartsWith line = or $ fmap (`T.isPrefixOf` T.strip line) patterns
