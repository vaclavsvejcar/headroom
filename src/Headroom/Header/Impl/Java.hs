{-|
Module      : Headroom.Header.Impl.Java
Description : Support for license header in Java files
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for detecting license header in /Java/ source code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Java
  ( headerSizeJava
  )
where

import           Headroom.Header.Utils          ( findLineStartingWith )
import           RIO
import           RIO.Text                       ( Text )


-- | Returns size of license header (as number of lines) in given /Java/
-- source code. Current implementation is pretty simple and it only takes line
-- starting with one of the following keywords as the start of code itself:
--
--   * @package@
headerSizeJava :: Text -> Int
headerSizeJava = findLineStartingWith ["package"]
