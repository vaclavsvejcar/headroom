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
  , readFileType
  )
where

import           Headroom.Types.Utils           ( allValues
                                                , readEnumCI
                                                )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Read                      ( readsPrec )


data FileType
  = Haskell
  | HTML
  | Java
  | Scala
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Read FileType where
  readsPrec _ = readEnumCI

fileTypeByExt :: T.Text -> Maybe FileType
fileTypeByExt ext =
  L.find (elem ext . listExtensions) (allValues :: [FileType])

listExtensions :: FileType -> [T.Text]
listExtensions Haskell = ["hs"]
listExtensions HTML    = ["html", "htm"]
listExtensions Java    = ["java"]
listExtensions Scala   = ["scala"]

readFileType :: T.Text -> Maybe FileType
readFileType = readMaybe . T.unpack
