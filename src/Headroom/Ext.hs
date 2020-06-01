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
  )
where

import qualified Headroom.Ext.Haskell          as Haskell
import           Headroom.Types                 ( CurrentYear
                                                , FileType(..)
                                                , HeaderConfig
                                                , Variables
                                                )
import           RIO


-- | Extracts variables specific to the file type (if supported), e.g. module
-- name for /Haskell/ source code. Currently supported file types are:
--
-- * /Haskell/ - implemented in "Headroom.FileSupport.Haskell"
extractVariables :: FileType
                 -- ^ type of the file
                 -> HeaderConfig
                 -- ^ license header configuration
                 -> Maybe (Int, Int)
                 -- ^ license header position @(startLine, endLine)@
                 -> CurrentYear
                 -- ^ current year
                 -> Text
                 -- ^ text of the source code file
                 -> Variables
                 -- ^ extracted variables
extractVariables fileType config headerPos year text = case fileType of
  Haskell -> Haskell.extractVariables config headerPos year text
  _       -> mempty
