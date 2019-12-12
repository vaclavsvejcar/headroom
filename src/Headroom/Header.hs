{-|
Module      : Headroom.Header
Description : License header manipulation
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and functions for adding and replacing license headers to source
code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header
  ( Header(..)
  , addHeader
  , containsHeader
  , headerSize
  , replaceHeader
  , stripHeader
  )
where

import           Headroom.FileType              ( FileType(..) )
import           Headroom.Header.Impl
import qualified Headroom.Text                 as T
import           Headroom.Types                 ( NewLine(..) )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


data Header = Header
  { hFileType :: FileType
  , hContent  :: T.Text
  }
  deriving (Eq, Show)

addHeader :: Header -> T.Text -> T.Text
addHeader (Header fileType content) input = output
 where
  output = if containsHeader' then input else content <> newLine <> input
  containsHeader' = containsHeader fileType input
  newLine = T.showNewLine $ fromMaybe LF (T.detectNewLine input)

containsHeader :: FileType -> T.Text -> Bool
containsHeader fileType input = headerSize fileType input > 0

headerSize :: FileType -> T.Text -> Int
headerSize Haskell = headerSizeHaskell
headerSize HTML    = headerSizeHTML
headerSize Java    = headerSizeJava
headerSize Scala   = headerSizeScala

replaceHeader :: Header -> T.Text -> T.Text
replaceHeader h@(Header fileType _) = addHeader h . stripHeader fileType

stripHeader :: FileType -> T.Text -> T.Text
stripHeader fileType input = T.unlines' newLine . L.drop numLines $ lines'
 where
  numLines          = headerSize fileType input
  (newLine, lines') = T.lines' input
