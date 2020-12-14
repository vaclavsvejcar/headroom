{-# LANGUAGE NoImplicitPrelude #-}

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
  ( extractVariables
  , extractTemplateMeta
  )
where

import           Headroom.Configuration.Types   ( CtHeaderConfig )
import qualified Headroom.Ext.Haskell          as Haskell
import qualified Headroom.Ext.Java             as Java
import qualified Headroom.Ext.PureScript       as PureScript
import           Headroom.FileType.Types        ( FileType(..) )
import           Headroom.TemplateSupport       ( TemplateSupport(..) )
import           Headroom.Types                 ( TemplateMeta(..) )
import           Headroom.Variables.Types       ( Variables(..) )
import           RIO


-- | Extracts variables specific to the file type (if supported), e.g. module
-- name for /Haskell/ source code. Currently supported file types are:
--
-- * /Haskell/ - implemented in "Headroom.Ext.Haskell"
-- * /Java/ - implemented in "Headroom.Ext.Java"
-- * /PureScript/ - implemented in "Headroom.Ext.PureScript"
extractVariables :: FileType
                 -- ^ type of the file
                 -> CtHeaderConfig
                 -- ^ license header configuration
                 -> Maybe TemplateMeta
                 -- ^ extracted metadata from corresponding /template/
                 -> Maybe (Int, Int)
                 -- ^ license header position @(startLine, endLine)@
                 -> Text
                 -- ^ text of the source code file
                 -> Variables
                 -- ^ extracted variables
extractVariables fileType = case fileType of
  Haskell    -> Haskell.extractVariables
  Java       -> Java.extractVariables
  PureScript -> PureScript.extractVariables
  _          -> mempty


-- | Extracts medatata from given /template/ for selected /file type/, which
-- might be later required by the 'extractVariables' function.
extractTemplateMeta :: TemplateSupport t
                    => FileType
                    -- ^ /file type/ for which this template will be used
                    -> t
                    -- ^ parsed /template/
                    -> Maybe TemplateMeta
                    -- ^ extracted template metadata
extractTemplateMeta Haskell tmpl = Just $ Haskell.extractTemplateMeta tmpl
extractTemplateMeta _       _    = Nothing
