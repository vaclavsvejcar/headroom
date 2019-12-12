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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.Header.Impl.HTML
  ( headerSizeHTML
  )
where

import           Headroom.Header.Utils          ( linesCountByRegex
                                                , reML
                                                )
import           RIO
import qualified RIO.Text                      as T


-- | Returns size of license header (as number of lines) in given /HTML/ source
-- code. The very first /HTML/ comment is considered as license header, anything
-- after as start of the actual code.
headerSizeHTML :: T.Text -> Int
headerSizeHTML = linesCountByRegex [reML|(<!--(?:.*?)-->)\s*|]
