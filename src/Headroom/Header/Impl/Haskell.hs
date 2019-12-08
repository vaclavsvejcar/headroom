{-|
Module      : Headroom.Header.Impl.Haskell
Description : License header - Haskell
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for detecting license header in /Haskell/ source code files.
-}
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
