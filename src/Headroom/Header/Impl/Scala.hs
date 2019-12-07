{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Scala where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


headerSizeScala :: T.Text -> Int
headerSizeScala = findLine
 where
  patterns = ["class", "object", "package"]
  findLine text = findIndex lineStartsWith (T.lines text)
  findIndex cond xs = fromMaybe 0 (L.findIndex cond xs)
  lineStartsWith line = or $ fmap (`T.isPrefixOf` T.strip line) patterns
