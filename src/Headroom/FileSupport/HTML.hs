{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
Module      : Headroom.FileSupport.HTML
Description : Support for /HTML/ source code files
Copyright   : (c) 2019-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Basic support for /HTML/ source code files. This implementation doesn't extract
any variables or template data.
-}

module Headroom.FileSupport.HTML
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
import           Headroom.FileType.Types             ( FileType(HTML) )


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /HTML/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport HTML syntaxAnalysis


------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis = SyntaxAnalysis { saIsCommentStart = isMatch [re|^<!--|]
                                , saIsCommentEnd   = isMatch [re|-->$|]
                                }
