{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.Types
-- Description : Data types for "Headroom.FileSupport" module
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Data types for "Headroom.FileSupport" module in separated module
-- (to avoid cyclic dependencies).
module Headroom.FileSupport.Types (
    -- * Data Types
      FileSupport (..)
    , SyntaxAnalysis (..)

      -- * Smart Constructors
    , defaultFileSupport

      -- * Function Type Aliases
    , ExtractTemplateDataFn
    , ExtractVariablesFn
) where

import Headroom.Config.Types (HeaderSyntax)
import Headroom.FileSupport.TemplateData (TemplateData (..))
import Headroom.FileType.Types (FileType)
import Headroom.Header.Types (HeaderTemplate)
import Headroom.SourceCode (SourceCode)
import Headroom.Template (Template)
import Headroom.Variables.Types (Variables)
import RIO

-- | Set of functions that every file support needs to implement.
data FileSupport = FileSupport
    { fsSyntaxAnalysis :: SyntaxAnalysis
    , fsExtractTemplateData :: ExtractTemplateDataFn
    , fsExtractVariables :: ExtractVariablesFn
    , fsFileType :: FileType
    }

-- | Set of functions used to analyze source code.
data SyntaxAnalysis = SyntaxAnalysis
    { saIsCommentStart :: Text -> Bool
    , saIsCommentEnd :: Text -> Bool
    }

-- | Type of a function that extracts additional template data from template.
type ExtractTemplateDataFn =
    forall a.
    Template a =>
    -- | template to use for extraction
    a ->
    -- | copyright header syntax
    HeaderSyntax ->
    -- | extracted template data
    TemplateData

-- | Type of a function that extracts variables from analyzed source code file.
type ExtractVariablesFn =
    -- | header template
    HeaderTemplate ->
    -- | header position as @(startLine, endLine)@
    Maybe (Int, Int) ->
    -- | analyzed source code file
    SourceCode ->
    -- | extracted variables
    Variables

-- | Default implementation of 'FileSupport' that doesn't extract any variables
-- or template data.
defaultFileSupport ::
    -- | type of the source code file
    FileType ->
    -- | function that analyzes source code
    SyntaxAnalysis ->
    -- | resulting 'FileSupport'
    FileSupport
defaultFileSupport fileType syntaxAnalysis =
    FileSupport
        { fsSyntaxAnalysis = syntaxAnalysis
        , fsExtractTemplateData = const . const $ NoTemplateData
        , fsExtractVariables = const . const . const $ mempty
        , fsFileType = fileType
        }
