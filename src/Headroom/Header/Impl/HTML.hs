{-|
Module      : Headroom.Header.Impl.HTML
Description : Support for license header in HTML files
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for detecting license header in /HTML/ source code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.Header.Impl.HTML
  ( headerSizeHTML
  )
where

import           Headroom.Header.Utils          ( linesCountByRegex
                                                , reML
                                                )
import           RIO
import           RIO.Text                       ( Text )


-- | Returns size of license header (as number of lines) in given /HTML/ source
-- code. The very first /HTML/ comment is considered as license header, anything
-- after as start of the actual code.
headerSizeHTML :: Text -> Int
headerSizeHTML = linesCountByRegex [reML|(<!--(?:.*?)-->)\s*|]
