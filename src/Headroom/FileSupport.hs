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

import qualified Headroom.FileSupport.Haskell       as Haskell
import qualified Headroom.FileSupport.Java          as Java
import qualified Headroom.FileSupport.PureScript    as PureScript
import           Headroom.FileSupport.TemplateData   ( TemplateData(..) )
import           Headroom.FileSupport.Types          ( FileSupport(..) )
import           Headroom.FileType.Types             ( FileType(..) )
import           RIO


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Returns 'FileSupport' for corresponding 'FileType'.
fileSupport :: FileType -> FileSupport
fileSupport C          = basicSupport C
fileSupport CPP        = basicSupport CPP
fileSupport CSS        = basicSupport CSS
fileSupport Haskell    = Haskell.fileSupport
fileSupport HTML       = basicSupport HTML
fileSupport JS         = basicSupport JS
fileSupport Java       = Java.fileSupport
fileSupport PureScript = PureScript.fileSupport
fileSupport Rust       = basicSupport Rust
fileSupport Scala      = basicSupport Scala
fileSupport Shell      = basicSupport Shell


------------------------------  PRIVATE FUNCTIONS  -----------------------------

basicSupport :: FileType -> FileSupport
basicSupport fileType = FileSupport
  { fsExtractTemplateData = const NoTemplateData
  , fsExtractVariables    = const . const . const $ mempty
  , fsFileType            = fileType
  }
