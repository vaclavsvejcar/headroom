{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.FileSupport
Description : Support for handling various source code file types
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

/Headroom/ currently supports working with file types defined in 'FileType'
type, and because every type of source code file requires different handling of
some aspects, this file type specific support is implemented for every supported
file type and exposed as instance of 'FileSupport' data type.
-}

module Headroom.FileSupport
  ( fileSupport
  )
where

import qualified Headroom.FileSupport.C             as C
import qualified Headroom.FileSupport.CPP           as CPP
import qualified Headroom.FileSupport.CSS           as CSS
import qualified Headroom.FileSupport.Haskell       as Haskell
import qualified Headroom.FileSupport.HTML          as HTML
import qualified Headroom.FileSupport.Java          as Java
import qualified Headroom.FileSupport.JS            as JS
import qualified Headroom.FileSupport.PureScript    as PureScript
import qualified Headroom.FileSupport.Rust          as Rust
import qualified Headroom.FileSupport.Scala         as Scala
import qualified Headroom.FileSupport.Shell         as Shell
import           Headroom.FileSupport.Types          ( FileSupport(..) )
import           Headroom.FileType.Types             ( FileType(..) )


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Returns 'FileSupport' for corresponding 'FileType'.
fileSupport :: FileType -> FileSupport
fileSupport C          = C.fileSupport
fileSupport CPP        = CPP.fileSupport
fileSupport CSS        = CSS.fileSupport
fileSupport Haskell    = Haskell.fileSupport
fileSupport HTML       = HTML.fileSupport
fileSupport Java       = Java.fileSupport
fileSupport JS         = JS.fileSupport
fileSupport PureScript = PureScript.fileSupport
fileSupport Rust       = Rust.fileSupport
fileSupport Scala      = Scala.fileSupport
fileSupport Shell      = Shell.fileSupport
