{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Headroom.FileSupport.Java
Description : Support for /Java/ source code files
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for /Java/ source code files. This implementation extracts /Java/
package name as variable.

= Extracted Variables for Templates
This implementation extracts following variables from source code file:

* @___java_package_name__@ - name of the /Java/ package

= Extracted Custom Data
This implementation does not extract any custom data from template file.
-}

module Headroom.FileSupport.Java
  ( fileSupport
  )
where

import           Headroom.Data.Regex                 ( match
                                                     , re
                                                     )
import           Headroom.Data.TextExtra             ( toLines )
import           Headroom.FileSupport.TemplateData   ( TemplateData(..) )
import           Headroom.FileSupport.Types          ( FileSupport(..) )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.Types               ( HeaderTemplate )
import           Headroom.Variables                  ( mkVariables )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import           RIO.Lens                            ( ix )


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Java/.
fileSupport :: FileSupport
fileSupport = FileSupport { fsExtractTemplateData = const NoTemplateData
                          , fsExtractVariables    = extractVariables
                          , fsFileType            = Java
                          }


------------------------------  PRIVATE FUNCTIONS  -----------------------------

extractPackageName :: Text -> Maybe Text
extractPackageName = go . toLines
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? ix 1) (match [re|^package (.*);$|] x)


extractVariables :: HeaderTemplate -> Maybe (Int, Int) -> Text -> Variables
extractVariables _ _ text = (mkVariables . catMaybes)
  [("_java_package_name", ) <$> extractPackageName text]
