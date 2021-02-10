{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
Module      : Headroom.FileSupport.Scala
Description : Support for /Scala/ source code files
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Basic support for /Scala/ source code files. This implementation doesn't extract
any variables or template data.
-}

module Headroom.FileSupport.Scala
  ( fileSupport
  )
where

import           Headroom.Data.Regex                 ( isMatch
                                                     , re
                                                     )
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     , defaultFileSupport
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Scala/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport Scala syntaxAnalysis


------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis = SyntaxAnalysis { saIsCommentStart = isMatch [re|^\/\*|^\/\/|]
                                , saIsCommentEnd   = isMatch [re|\*\/$|^\/\/|]
                                }
