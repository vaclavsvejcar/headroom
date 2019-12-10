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

import           Headroom.Header.Utils          ( reML )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Heavy


-- | Returns size of license header (as number of lines) in given /HTML/ source
-- code. Anything that is not comment or blank line is considered as start of
-- the code itself.
headerSizeHTML :: T.Text -> Int
headerSizeHTML text =
  case L.headMaybe $ scan [reML|(<!--(?:.*?)-->)\s*|] text of
    Just (comment, _) -> L.length . T.lines $ comment
    _                 -> 0
