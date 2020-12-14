{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Headroom.Ext.Java
Description : Extended support for /Java/ source code files
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides /extended support/ for /Java/ source code files. Current
implementation includes extracting of /package name/ from currently processed
/Java/ source code file.
-}

module Headroom.Ext.Java
  ( extractVariables
  , extractPackageName
  )
where

import           Headroom.Configuration.Types        ( CtHeaderConfig )
import           Headroom.Data.Regex                 ( match
                                                     , re
                                                     )
import           Headroom.Data.TextExtra             ( toLines )
import           Headroom.Types                      ( TemplateMeta(..) )
import           Headroom.Variables                  ( mkVariables )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import           RIO.Lens                            ( ix )


-- | Extracts name of /Java/ package from given source code file content.
--
-- >>> extractPackageName "package mypackage;\nclass Foo {}"
-- Just "mypackage"
extractPackageName :: Text
                   -- ^ input text
                   -> Maybe Text
                   -- ^ extracted package name
extractPackageName = go . toLines
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? ix 1) (match [re|^package (.*);$|] x)


-- | Extracts variables from /Java/ source code.
--
-- __List of Extracted Variables:__
--
-- * @___java_package_name__@ - name of the /Java/ package
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
  [("_java_package_name", ) <$> extractPackageName text]
