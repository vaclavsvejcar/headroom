{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Headroom.Ext.Haskell
Description : Extended support for /Haskell/ source code files
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides support for extracting /Haskell/-specific info from the
parsed /source code files/ as /template variables/. Such info includes
/module name/ and selected fields from /Haddock module header/
(see "Headroom.Header.Haskell.Haddock").
-}

module Headroom.Ext.Haskell
  ( -- * Variables Extraction
    extractModuleName
  , extractVariables
    -- * Template Metadata Extraction
  , extractTemplateMeta
  )
where

import           Headroom.Configuration.Types        ( CtHeaderConfig )
import           Headroom.Data.Regex                 ( match
                                                     , re
                                                     )
import           Headroom.Data.TextExtra             ( fromLines
                                                     , toLines
                                                     )
import           Headroom.Ext.Haskell.Haddock        ( HaddockModuleHeader(..)
                                                     , extractFieldOffsets
                                                     , extractModuleHeader
                                                     )
import           Headroom.TemplateSupport            ( TemplateSupport(..) )
import           Headroom.Types                      ( TemplateMeta(..) )
import           Headroom.Variables                  ( mkVariables )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import           RIO.Lens                            ( ix )
import qualified RIO.List                           as L
import qualified RIO.Text                           as T


-- | Extracts name of /Haskell/ module from given source code file content.
--
-- >>> extractModuleName "{-# LANGUAGE OverloadedStrings #-}\nmodule Foo where"
-- Just "Foo"
extractModuleName :: Text
                  -- ^ input text
                  -> Maybe Text
                  -- ^ extracted module name
extractModuleName = go . toLines
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? ix 1) (match [re|^module\s+(\S+)|] x)


-- | Extracts variables from /Haskell/ source code.
--
-- __List of Extracted Variables:__
--
-- * @___haskell_module_copyright__@ - @Copyright@ field of /Haddock/ module header
-- * @___haskell_module_license__@ - @License@ field of /Haddock/ module header
-- * @___haskell_module_maintainer__@ - @Maintainer@ field of /Haddock/ module header
-- * @___haskell_module_portability__@ - @Portability@ field of /Haddock/ module header
-- * @___haskell_module_stability__@ - @Stability@ field of /Haddock/ module header
-- * @___haskell_module_name__@ - name of the /Haskell/ module
-- * @___haskell_module_longdesc__@ - long description of /Haddock/ module
-- * @___haskell_module_shortdesc__@ - @Description@ field of /Haddock/ module header
extractVariables :: CtHeaderConfig
                 -- ^ license header configuration
                 -> Maybe TemplateMeta
                 -- ^ extracted metadata from corresponding /template/
                 -> Maybe (Int, Int)
                 -- ^ license header position @(startLine, endLine)@
                 -> Text
                 -- ^ input text
                 -> Variables
                 -- ^ extracted variables
extractVariables _ meta headerPos text = (mkVariables . catMaybes)
  [ ("_haskell_module_copyright", ) <$> hmhCopyright
  , ("_haskell_module_license", ) <$> hmhLicense
  , ("_haskell_module_maintainer", ) <$> hmhMaintainer
  , ("_haskell_module_name", ) <$> extractModuleName text
  , ("_haskell_module_portability", ) <$> hmhPortability
  , ("_haskell_module_stability", ) <$> hmhStability
  , ("_haskell_module_longdesc", ) <$> hmhLongDesc
  , ("_haskell_module_shortdesc", ) <$> hmhShortDesc
  ]
 where
  HaddockModuleHeader {..} = extractModuleHeader headerText meta
  headerText               = maybe T.empty (\(s, e) -> cut s e text) headerPos
  cut s e = fromLines . L.take (e - s) . L.drop s . toLines


-- | Extracts template metadata specific for /Haskell/.
extractTemplateMeta :: TemplateSupport t
                    => t
                    -- ^ parsed /template/
                    -> TemplateMeta
                    -- ^ extracted template metadata
extractTemplateMeta template = HaskellTemplateMeta offsets
  where offsets = extractFieldOffsets template
