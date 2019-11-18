{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header
  ( headerSize
  , stripHeader
  )
where

import           Headroom.Header.All
import           Headroom.Types                 ( FileType(..) )
import qualified Headroom.Text                 as T
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T

headerSize :: FileType -> T.Text -> Int
headerSize Haskell = headerSizeHaskell

stripHeader :: FileType -> T.Text -> T.Text
stripHeader fileType text = T.unlines' newLine . L.drop numLines $ lines'
 where
  numLines          = headerSize fileType text
  (newLine, lines') = T.lines' text
