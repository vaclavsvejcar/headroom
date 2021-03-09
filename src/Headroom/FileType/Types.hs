{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.FileType.Types
Description : Data types for "Headroom.FileType"
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types for "Headroom.FileType" modules.
-}

module Headroom.FileType.Types where

import           Headroom.Data.EnumExtra             ( EnumExtra(..) )
import           RIO

-- | Supported type of source code file.
data FileType
  = C
  -- ^ support for /C/ programming language
  | CPP
  -- ^ support for /C++/ programming language
  | CSS
  -- ^ support for /CSS/
  | Go
  -- ^ support for /Go/ programming language
  | Haskell
  -- ^ support for /Haskell/ programming language
  | HTML
  -- ^ support for /HTML/
  | Java
  -- ^ support for /Java/ programming language
  | JS
  -- ^ support for /JavaScript/ programming language
  | PureScript
  -- ^ support for /PureScript/ programming language
  | Rust
  -- ^ support for /Rust/ programming language
  | Scala
  -- ^ support for /Scala/ programming language
  | Shell
  -- ^ support for /Shell/
  deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)
