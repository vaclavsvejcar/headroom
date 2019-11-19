{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileType
  ( listExtensions
  , parseFileType
  )
where

import           Headroom.Types                 ( FileType(..) )
import           RIO
import qualified RIO.Text                      as T


listExtensions :: FileType -> [T.Text]
listExtensions Haskell = ["hs"]

parseFileType :: T.Text -> Maybe FileType
parseFileType = readMaybe . T.unpack
