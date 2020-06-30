{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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


