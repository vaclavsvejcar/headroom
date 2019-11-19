{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header
  ( headerSize
  , stripHeader
  )
where

import           Headroom.Header.All
import           Headroom.Types                 ( FileType(..)
                                                , Header(..)
                                                )
import qualified Headroom.Text                 as T
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T

headerSize :: Header -> Int
headerSize (Header Haskell text) = headerSizeHaskell text

stripHeader :: Header -> T.Text
stripHeader h@(Header _ text) = T.unlines' newLine . L.drop numLines $ lines'
 where
  numLines          = headerSize h
  (newLine, lines') = T.lines' text
