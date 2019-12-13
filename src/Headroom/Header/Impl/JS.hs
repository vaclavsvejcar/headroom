{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.Header.Impl.JS
  ( headerSizeJS
  )
where

import           Headroom.Header.Utils          ( linesCountByRegex
                                                , reML
                                                )
import           RIO
import qualified RIO.Text                      as T

headerSizeJS :: T.Text -> Int
headerSizeJS = linesCountByRegex [reML|(\/\*(?:.*?)\*\/)\s*|(\s*)|]
