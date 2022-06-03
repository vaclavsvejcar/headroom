{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileType
-- Description : Logic for handlig supported file types
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module providing functions for working with the 'FileType', such as performing
-- detection based on the file extension, etc.
module Headroom.FileType (
    configByFileType
    , fileTypeByExt
    , listExtensions
) where

import Headroom.Config.Types (
    CtHeaderConfig
    , CtHeadersConfig
    , HeaderConfig (..)
    , HeadersConfig (..)
 )
import Headroom.Data.EnumExtra (EnumExtra (..))
import Headroom.FileType.Types (FileType (..))
import RIO
import qualified RIO.List as L

-- | Returns 'FileType' for given file extension (without dot), using configured
-- values from the 'HeadersConfig'.
fileTypeByExt ::
    -- | license headers configuration
    CtHeadersConfig ->
    -- | file extension (without dot)
    Text ->
    -- | found 'FileType'
    Maybe FileType
fileTypeByExt config ext =
    L.find (elem ext . listExtensions config) (allValues @FileType)

-- | Lists all recognized file extensions for given 'FileType', using configured
-- values from the 'HeadersConfig'.
listExtensions ::
    -- | license headers configuration
    CtHeadersConfig ->
    -- | 'FileType' for which to list extensions
    FileType ->
    -- | list of appropriate file extensions
    [Text]
listExtensions config fileType =
    hcFileExtensions (configByFileType config fileType)

-- | Returns the proper 'HeaderConfig' for the given 'FileType', selected
-- from the 'HeadersConfig'.
configByFileType ::
    -- | license headers configuration
    CtHeadersConfig ->
    -- | selected 'FileType'
    FileType ->
    -- | appropriate 'HeaderConfig'
    CtHeaderConfig
configByFileType HeadersConfig{..} fileType = case fileType of
    C -> hscC
    CPP -> hscCpp
    CSS -> hscCss
    Dart -> hscDart
    Go -> hscGo
    Haskell -> hscHaskell
    HTML -> hscHtml
    Java -> hscJava
    JS -> hscJs
    Kotlin -> hscKotlin
    PHP -> hscPhp
    PureScript -> hscPureScript
    Python -> hscPython
    Rust -> hscRust
    Scala -> hscScala
    Shell -> hscShell
