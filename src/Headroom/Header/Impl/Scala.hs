{-|
Module      : Headroom.Header.Impl.Scala
Description : License header - Scala
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for detecting license header in /Scala/ source code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Scala where

import           Headroom.Header.Utils          ( findLineStartingWith )
import           RIO
import qualified RIO.Text                      as T


headerSizeScala :: T.Text -> Int
headerSizeScala = findLineStartingWith ["class", "object", "package"]
