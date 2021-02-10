{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Headroom.FileSupport.PureScript
Description : Support for /PureScript/ source code files
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for /PureScript/ source code files. This implementation extracts
/PureScript/ module name as variable.

= Extracted Variables for Templates
This implementation extracts following variables from source code file:

* @___purescript_module_name__@ - name of the /PureScript/ module

= Extracted Custom Data
This implementation does not extract any custom data from template file.
-}

module Headroom.FileSupport.PureScript
  ( fileSupport
  )
where

import           Headroom.Data.Regex                 ( isMatch
                                                     , match
                                                     , re
                                                     )
import           Headroom.Data.Text                  ( toLines )
import           Headroom.FileSupport.TemplateData   ( TemplateData(..) )
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.Types               ( HeaderTemplate )
import           Headroom.Variables                  ( mkVariables )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import           RIO.Lens                            ( ix )


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /PureScript/.
fileSupport :: FileSupport
fileSupport = FileSupport { fsSyntaxAnalysis      = syntaxAnalysis
                          , fsExtractTemplateData = const NoTemplateData
                          , fsExtractVariables    = extractVariables
                          , fsFileType            = PureScript
                          }


------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis = SyntaxAnalysis
  { saIsCommentStart = isMatch [re|^{-(?!\h*#)|^--|]
  , saIsCommentEnd   = isMatch [re|^\h*-}|\w+\h*-}|^--|]
  }


extractVariables :: HeaderTemplate -> Maybe (Int, Int) -> Text -> Variables
extractVariables _ _ text = (mkVariables . catMaybes)
  [("_purescript_module_name", ) <$> extractModuleName text]


extractModuleName :: Text -> Maybe Text
extractModuleName = go . toLines
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? ix 1) (match [re|^module\s+(\S+)|] x)

