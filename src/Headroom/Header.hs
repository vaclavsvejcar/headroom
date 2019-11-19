{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header
  ( addHeader
  , headerSize
  , replaceHeader
  , stripHeader
  )
where

import           Headroom.Header.All
import           Headroom.Types                 ( FileType(..)
                                                , Header(..)
                                                , NewLine(..)
                                                )
import qualified Headroom.Text                 as T
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


addHeader :: Header -> T.Text -> T.Text
addHeader (Header _ content) input = content <> newLine <> input
  where newLine = T.showNewLine $ fromMaybe LF (T.detectNewLine input)

headerSize :: FileType -> T.Text -> Int
headerSize Haskell = headerSizeHaskell

replaceHeader :: Header -> T.Text -> T.Text
replaceHeader h@(Header fileType _) = addHeader h . stripHeader fileType

stripHeader :: FileType -> T.Text -> T.Text
stripHeader fileType input = T.unlines' newLine . L.drop numLines $ lines'
 where
  numLines          = headerSize fileType input
  (newLine, lines') = T.lines' input
