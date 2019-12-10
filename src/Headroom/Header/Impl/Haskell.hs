{-|
Module      : Headroom.Header.Impl.Haskell
Description : Support for license header in Haskell files
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


-- | Returns size of license header (as number of lines) in given /Haskell/
-- source code. Current implementation is pretty simple and it only takes line
-- starting with one of the following keywords as the start of code itself:
--
--   * @{-#@
--   * @module@
headerSizeHaskell :: T.Text -> Int
headerSizeHaskell = findLineStartingWith ["{-#", "module"]
