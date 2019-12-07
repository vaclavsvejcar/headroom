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
findLineStartingWith patterns text = findLine predicate text
  where predicate line = or $ fmap (`T.isPrefixOf` line) patterns
