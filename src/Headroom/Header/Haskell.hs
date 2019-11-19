{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Haskell
  ( headerSizeHaskell
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.List                      as L


headerSizeHaskell :: T.Text -> Int
headerSizeHaskell = findLine
 where
  patterns = ["{-#", "module"]
  findLine text = findIndex lineStartsWith (T.lines text)
  findIndex cond xs = fromMaybe 0 (L.findIndex cond xs)
  lineStartsWith line = or $ fmap (`T.isPrefixOf` T.strip line) patterns
