{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.FileSupport.Types
Description : Data types for "Headroom.FileSupport" module
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types for "Headroom.FileSupport" module in separated module
(to avoid cyclic dependencies).
-}

module Headroom.FileSupport.Types
  ( -- * Data Types
    FileSupport(..)
    -- * Function Type Aliases
  , ExtractTemplateDataFn
  , ExtractVariablesFn
  )
where

import           Headroom.FileSupport.TemplateData   ( TemplateData )
import           Headroom.FileType.Types             ( FileType )
import           Headroom.Header.Types               ( TemplateInfo )
import           Headroom.Template                   ( Template )
import           Headroom.Variables.Types            ( Variables )
import           RIO


-- | Represents set of functions that every file support needs to implement.
data FileSupport = FileSupport
  { fsExtractTemplateData :: ExtractTemplateDataFn
  , fsExtractVariables    :: ExtractVariablesFn
  , fsFileType            :: FileType
  }


-- | Type of a function that extracts additional template data from template.
type ExtractTemplateDataFn
  =  forall a
   . Template a
  => a
  -- ^ template to use for extraction
  -> TemplateData
  -- ^ extracted template data


-- | Type of a function that extracts variables from processed source code file.
type ExtractVariablesFn
  =  TemplateInfo
  -- ^ template info
  -> Maybe (Int, Int)
  -- ^ header position as @(startLine, endLine)@
  -> Text
  -- ^ text of processed source code file
  -> Variables
  -- ^ extracted variables
