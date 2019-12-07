{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Haskell
  ( headerSizeHaskell
  )
where

import           Headroom.Header.Utils          ( findLineStartingWith )
import           RIO
import qualified RIO.Text                      as T


headerSizeHaskell :: T.Text -> Int
headerSizeHaskell = findLineStartingWith ["{-#", "module"]
