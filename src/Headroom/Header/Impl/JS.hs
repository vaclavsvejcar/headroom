{-|
Module      : Headroom.Header.Impl.JS
Description : Support for license header in JavaScript files
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for detecting license header in /JavaScript/ source code files.
-}
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
import           RIO.Text                       ( Text )

headerSizeJS :: Text -> Int
headerSizeJS = linesCountByRegex [reML|(\/\*(?:.*?)\*\/)\s*|(\s*)|]
