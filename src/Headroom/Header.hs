{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Header
  ( headerSize
  )
where

import Headroom.Header.All
import Headroom.Types (FileType(..))
import           RIO
import qualified RIO.Text                      as T

headerSize :: FileType -> T.Text -> Int
headerSize Haskell = headerSizeHaskell
