{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Headroom.Header.TemplateInfo
Description : Functions for working with license header templates
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions for working with 'TemplateInfo' data type.
-}

module Headroom.Header.TemplateInfo
  ( mkTemplateInfo
  )
where

import           Headroom.Configuration.Types        ( CtHeadersConfig )
import           Headroom.Ext                        ( extractExtData )
import           Headroom.FileType                   ( configByFileType )
import           Headroom.FileType.Types             ( FileType )
import           Headroom.Header.Types               ( TemplateInfo(..) )
import           Headroom.Meta                       ( TemplateType )


-- | Constructs new 'TemplateInfo' from provided data.
mkTemplateInfo :: CtHeadersConfig
               -- ^ configuration for license headers
               -> FileType
               -- ^ type of source code files this template is for
               -> TemplateType
               -- ^ parsed template
               -> TemplateInfo
               -- ^ resulting template info
mkTemplateInfo configs fileType template =
  let tiConfig   = configByFileType configs fileType
      tiExtData  = extractExtData fileType template
      tiFileType = fileType
      tiTemplate = template
  in  TemplateInfo { .. }
