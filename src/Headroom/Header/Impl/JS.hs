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

-- | Returns size of license header (as number of lines) in given /JS/ source
-- code. The very first comment block is considered as license header, anything
-- after as start of the actual code.
headerSizeJS :: Text -> Int
headerSizeJS = linesCountByRegex [reML|(\/\*(?:.*?)\*\/)\s*|(\s*)|]
