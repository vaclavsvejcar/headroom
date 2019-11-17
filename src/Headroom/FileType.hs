{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.FileType where

import           Headroom.Types                 ( FileType )
import           RIO
import qualified RIO.Text                      as T

parseFileType :: T.Text -> Maybe FileType
parseFileType = readMaybe . T.unpack
