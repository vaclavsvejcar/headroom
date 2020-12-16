{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Headroom.Ext
Description : Extended support for selected /file types/
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

/Headroom/ provides /extended support/ for selected /file types/, which mean
not only basic management of /license headers/ based on /template files/ is
provided, but also some additional functionality is available. For example,
some extra data may be extracted from the processed /source code file/ and made
available to template via /template variables/
(e.g. /module name/ for /Haskell source code files/).
-}

module Headroom.Ext
  ( extractExtData
  , extractVariables
  )
where

import qualified Headroom.Ext.Haskell               as Haskell
import qualified Headroom.Ext.Java                  as Java
import qualified Headroom.Ext.PureScript            as PureScript
import           Headroom.Ext.Types                  ( ExtData(..) )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.Types               ( TemplateInfo(..) )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO


-- | Extracts some data for extended support specific to the given 'FileType'
-- from the 'Template'.
extractExtData :: Template a
               => FileType
               -- ^ /file type/ for which this template will be used
               -> a
               -- ^ parsed /header template/
               -> ExtData
               -- ^ extracted data
extractExtData Haskell = Haskell.extractExtData
extractExtData _       = const NoExtData


-- | Extracts variables specific to the file type (if supported), e.g. module
-- name for /Haskell/ source code. Currently supported file types are:
--
-- * /Haskell/ - implemented in "Headroom.Ext.Haskell"
-- * /Java/ - implemented in "Headroom.Ext.Java"
-- * /PureScript/ - implemented in "Headroom.Ext.PureScript"
extractVariables :: TemplateInfo
                 -- ^ template info
                 -> Maybe (Int, Int)
                 -- ^ license header position @(startLine, endLine)@
                 -> Text
                 -- ^ text of the source code file
                 -> Variables
                 -- ^ extracted variables
extractVariables ti@(tiFileType -> Haskell   ) = Haskell.extractVariables ti
extractVariables ti@(tiFileType -> Java      ) = Java.extractVariables ti
extractVariables ti@(tiFileType -> PureScript) = PureScript.extractVariables ti
extractVariables _                             = mempty
