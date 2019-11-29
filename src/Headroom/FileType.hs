{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileType
  ( fileTypeByExt
  , listExtensions
  , readFileType
  )
where

import           Headroom.Types                 ( FileType(..) )
import           Headroom.Types.Util            ( allValues )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


fileTypeByExt :: T.Text -> Maybe FileType
fileTypeByExt ext =
  L.find (elem ext . listExtensions) (allValues :: [FileType])

listExtensions :: FileType -> [T.Text]
listExtensions Haskell = ["hs"]

readFileType :: T.Text -> Maybe FileType
readFileType = readMaybe . T.unpack
