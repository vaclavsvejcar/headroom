{-|
Module      : Headroom.FileType
Description : Supported source code file types
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and functions for representing and handling supported types of
source code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileType
  ( FileType(..)
  , fileTypeByExt
  , listExtensions
  , fileTypeByName
  )
where

import           Headroom.Types.Utils           ( allValues
                                                , readEnumCI
                                                )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Read                      ( readsPrec )


-- | Represents supported type of source code file, where license headers may
-- be added, replaced or removed.
data FileType
  = CSS     -- ^ /CSS/ source code file
  | Haskell -- ^ /Haskell/ source code file
  | HTML    -- ^ /HTML/ source code file
  | Java    -- ^ /Java/ source code file
  | JS      -- ^ /JavaScript/ source code file
  | Scala   -- ^ /Scala/ source code file
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Read FileType where
  readsPrec _ = readEnumCI

-- | Returns 'FileType' for given file extension (without dot).
--
-- >>> fileTypeByExt "hs"
-- Just Haskell
fileTypeByExt :: T.Text         -- ^ file extension to search for
              -> Maybe FileType -- ^ corresponding 'FileType' (if found)
fileTypeByExt ext =
  L.find (elem ext . listExtensions) (allValues :: [FileType])

-- | Lists all recognized file extensions for given 'Filetype'.
listExtensions :: FileType -- ^ 'FileType' to list extensions for
               -> [T.Text] -- ^ list of found file extensions
listExtensions CSS     = ["css"]
listExtensions Haskell = ["hs"]
listExtensions HTML    = ["html", "htm"]
listExtensions Java    = ["java"]
listExtensions JS      = ["js"]
listExtensions Scala   = ["scala"]

-- | Reads 'FileType' from its textual representation.
--
-- >>> fileTypeByName "haskell"
-- Just Haskell
fileTypeByName :: T.Text         -- ^ textual representation of 'FileType'
               -> Maybe FileType -- ^ corresponding 'FileType' (if found)
fileTypeByName = readMaybe . T.unpack
