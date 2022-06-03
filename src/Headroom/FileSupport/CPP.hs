{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.CPP
-- Description : Support for /C++/ source code files
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Basic support for /C++/ source code files. This implementation doesn't extract
-- any variables or template data.
module Headroom.FileSupport.CPP (
    fileSupport
) where

import Headroom.Data.Regex (
    isMatch
    , re
 )
import Headroom.FileSupport.Types (
    FileSupport (..)
    , SyntaxAnalysis (..)
    , defaultFileSupport
 )
import Headroom.FileType.Types (FileType (CPP))

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /C++/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport CPP syntaxAnalysis

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^\/\*|^\/\/|]
        , saIsCommentEnd = isMatch [re|\*\/$|^\/\/|]
        }
