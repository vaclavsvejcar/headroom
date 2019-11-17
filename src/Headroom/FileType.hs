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
listExtensions CSS     = ["css"]
listExtensions Haskell = ["hs"]
listExtensions HTML    = ["htm", "html"]

parseFileType :: T.Text -> Maybe FileType
parseFileType = readMaybe . T.unpack
