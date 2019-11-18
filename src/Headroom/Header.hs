{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header
  ( headerSize
  , stripHeader
  )
where

import           Headroom.Header.All
import           Headroom.Types                 ( FileType(..) )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T

headerSize :: FileType -> T.Text -> Int
headerSize Haskell = headerSizeHaskell

-- TODO proper line ending should be detected by input
stripHeader :: FileType -> T.Text -> T.Text
stripHeader fileType text = T.intercalate "\n" . L.drop num . T.lines $ text
  where num = headerSize fileType text
