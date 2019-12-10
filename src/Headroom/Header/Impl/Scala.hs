{-|
Module      : Headroom.Header.Impl.Scala
Description : Support for license header in Scala files
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for detecting license header in /Scala/ source code files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Impl.Scala
  ( headerSizeScala
  )
where

import           Headroom.Header.Utils          ( findLineStartingWith )
import           RIO
import qualified RIO.Text                      as T


-- | Returns size of license header (as number of lines) in given /Scala/
-- source code. Current implementation is pretty simple and it only takes line
-- starting with one of the following keywords as the start of code itself:
--
--   * @class@
--   * @object@
--   * @package@
headerSizeScala :: T.Text -> Int
headerSizeScala = findLineStartingWith ["class", "object", "package"]
