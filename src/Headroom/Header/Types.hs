{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Header.Types
Description : Data types for "Headroom.Header"
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types for "Headroom.Header" module.
-}

module Headroom.Header.Types
  ( -- * Data Types
    FileInfo(..)
  , TemplateInfo(..)
  )
where

import           Headroom.Configuration.Types        ( CtHeaderConfig )
import           Headroom.Ext.Types                  ( ExtData )
import           Headroom.FileType.Types             ( FileType )
import           Headroom.Meta                       ( TemplateType )
import           Headroom.Variables.Types            ( Variables )
import           RIO


-- | Info extracted about the concrete source code file.
data FileInfo = FileInfo
  { fiFileType     :: FileType
  -- ^ type of the file
  , fiHeaderConfig :: CtHeaderConfig
  -- ^ configuration for license header
  , fiHeaderPos    :: Maybe (Int, Int)
  -- ^ position of existing license header
  , fiVariables    :: Variables
  -- ^ additional extracted variables
  }
  deriving (Eq, Show)


-- | Represents info about concrete header template.
data TemplateInfo = TemplateInfo
  { tiConfig   :: CtHeaderConfig
  -- ^ header configuration
  , tiExtData  :: ExtData
  -- ^ additional data for /extended support/
  , tiFileType :: FileType
  -- ^ type of the file this template is for
  , tiTemplate :: TemplateType
  -- ^ parsed template
  }
  deriving (Eq, Show)
