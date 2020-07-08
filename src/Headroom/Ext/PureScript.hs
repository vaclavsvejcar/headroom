{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Headroom.Ext.PureScript
Description : Extended support for /PureScript/ source code files
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides /extended support/ for /PureScript/ source code files.
Current implementation includes extracting of /module name/ from currently
processed /PureScript/ source code file.
-}

module Headroom.Ext.PureScript where

import           Headroom.Configuration.Types   ( CtHeaderConfig )
import           Headroom.Ext.Haskell           ( extractModuleName )
import           Headroom.Types                 ( TemplateMeta(..) )
import           Headroom.Variables             ( mkVariables )
import           Headroom.Variables.Types       ( Variables(..) )
import           RIO


-- | Extracts variables from /PureScript/ source code.
--
-- __List of Extracted Variables:__
--
-- * @___purescript_module_name__@ - name of the /PureScript/ module
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
extractVariables _ _ _ text = (mkVariables . catMaybes)
  [("_purescript_module_name", ) <$> extractModuleName text]


