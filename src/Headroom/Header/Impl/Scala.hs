{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Scala where

import           Headroom.Header.Utils          ( findLineStartingWith )
import           RIO
import qualified RIO.Text                      as T


headerSizeScala :: T.Text -> Int
headerSizeScala = findLineStartingWith ["class", "object", "package"]
