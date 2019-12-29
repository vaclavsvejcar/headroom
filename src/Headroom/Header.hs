{-|
Module      : Headroom.Header
Description : License header manipulation
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

License header is usually the very top comment in source code, holding some
short text about license type, author and copyright. This module provides data
types and functions for adding, removing and replacing such headers. The license
header is represented by 'Header' data type, where 'FileType' defines for which
programming language source code this header is generated and the header text
itself.
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
import           RIO.Text                       ( Text )


-- | Generated license header for specified source code file type.
data Header = Header
  { hFileType :: FileType -- ^ type of the source code
  , hContent  :: Text     -- ^ text of the header
  }
  deriving (Eq, Show)

-- | Adds header to the given source code text if no existing header is
-- detected, otherwise returns the unchanged input text. If you need to replace
-- the header, use the 'replaceHeader' instead.
addHeader :: Header -- ^ license header to add to the input text
          -> Text   -- ^ source code text
          -> Text   -- ^ source code text with added license header
addHeader (Header fileType content) input = output
 where
  output = if containsHeader' then input else content <> newLine <> input
  containsHeader' = containsHeader fileType input
  newLine = T.showNewLine $ fromMaybe LF (T.detectNewLine input)

-- | Checks whether the license header is present in given source code text.
containsHeader :: FileType -- ^ type of the input source code text
               -> Text     -- ^ source code text
               -> Bool     -- ^ result of check
containsHeader fileType input = headerSize fileType input > 0

-- | Detects what is the header size in terms of lines in the given source code
-- text. Returns @0@ if no header detected.
headerSize :: FileType -- ^ type of the input source code text
           -> Text     -- ^ source code text
           -> Int      -- ^ size of the headers (number of lines)
headerSize CSS     = headerSizeCSS
headerSize Haskell = headerSizeHaskell
headerSize HTML    = headerSizeHTML
headerSize Java    = headerSizeJava
headerSize JS      = headerSizeJS
headerSize Scala   = headerSizeScala

-- | Replaces already existing (or adds if none detected) license header with
-- the new one in the given source code text. If you need to only add header if
-- none detected and skip if it already contains one, use the 'addHeader'
-- instead.
replaceHeader :: Header -- ^ new license header to use for replacement
              -> Text   -- ^ source code text
              -> Text   -- ^ source code text with replaced license header
replaceHeader h@(Header fileType _) = addHeader h . stripHeader fileType

-- | Strips license header (if detected) from the given source code text.
stripHeader :: FileType -- ^ type of the input source code text
            -> Text     -- ^ source code text
            -> Text     -- ^ source code text without the license header
stripHeader fileType input = T.unlines' newLine . L.drop numLines $ lines'
 where
  numLines          = headerSize fileType input
  (newLine, lines') = T.lines' input
