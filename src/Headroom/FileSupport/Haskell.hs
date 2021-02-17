{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Headroom.FileSupport.Haskell
Description : Support for /Haskell/ source code files
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Support for /Haskell/ source code files. This implementation extracts module
name and /Haddock/ fields as variables (see below). For more details about
/Haddock/ extraction, see "Headroom.FileSupport.Haskell.Haddock" module.

= Extracted Variables for Templates
This implementation extracts following variables from source code file:

* @___haskell_module_copyright__@ - @Copyright@ field of /Haddock/ module header
* @___haskell_module_license__@ - @License@ field of /Haddock/ module header
* @___haskell_module_maintainer__@ - @Maintainer@ field of /Haddock/ module header
* @___haskell_module_portability__@ - @Portability@ field of /Haddock/ module header
* @___haskell_module_stability__@ - @Stability@ field of /Haddock/ module header
* @___haskell_module_name__@ - name of the /Haskell/ module
* @___haskell_module_longdesc__@ - long description of /Haddock/ module
* @___haskell_module_shortdesc__@ - @Description@ field of /Haddock/ module header

= Extracted Custom Data
This implementation extracts custom data from used template, represented by the
'HaskellTemplateData'' data type.
-}

module Headroom.FileSupport.Haskell
  ( fileSupport
  )
where

import           Headroom.Data.Regex                 ( isMatch
                                                     , match
                                                     , re
                                                     )
import           Headroom.FileSupport.Haskell.Haddock
                                                     ( HaddockModuleHeader(..)
                                                     , extractModuleHeader
                                                     , extractOffsets
                                                     )
import           Headroom.FileSupport.TemplateData   ( HaskellTemplateData'(..)
                                                     , TemplateData(..)
                                                     )
import           Headroom.FileSupport.Types          ( FileSupport(..)
                                                     , SyntaxAnalysis(..)
                                                     )

import           Headroom.Data.Coerce                ( coerce )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.Types               ( HeaderTemplate(..) )
import           Headroom.SourceCode                 ( CodeLine
                                                     , SourceCode(..)
                                                     , cut
                                                     )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Variables                  ( mkVariables )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import           RIO.Lens                            ( ix )


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Haskell/.
fileSupport :: FileSupport
fileSupport = FileSupport { fsSyntaxAnalysis      = syntaxAnalysis
                          , fsExtractTemplateData = extractTemplateData
                          , fsExtractVariables    = extractVariables
                          , fsFileType            = Haskell
                          }


------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis = SyntaxAnalysis
  { saIsCommentStart = isMatch [re|^{-(?!\h*#)|^--|]
  , saIsCommentEnd   = isMatch [re|^\h*-}|\w+\h*-}|^--|]
  }


extractTemplateData :: Template a => a -> TemplateData
extractTemplateData template =
  let htdHaddockOffsets = extractOffsets template
      templateData      = HaskellTemplateData' { .. }
  in  HaskellTemplateData templateData


extractVariables :: HeaderTemplate
                 -> Maybe (Int, Int)
                 -> SourceCode
                 -> Variables
extractVariables HeaderTemplate {..} headerPos source =
  (mkVariables . catMaybes)
    [ ("_haskell_module_copyright", ) <$> hmhCopyright
    , ("_haskell_module_license", ) <$> hmhLicense
    , ("_haskell_module_maintainer", ) <$> hmhMaintainer
    , ("_haskell_module_name", ) <$> extractModuleName source
    , ("_haskell_module_portability", ) <$> hmhPortability
    , ("_haskell_module_stability", ) <$> hmhStability
    , ("_haskell_module_longdesc", ) <$> hmhLongDesc
    , ("_haskell_module_shortdesc", ) <$> hmhShortDesc
    ]
 where
  HaddockModuleHeader {..} = extractModuleHeader header htTemplateData
  header                   = maybe mempty (\(s, e) -> cut s e source) headerPos


extractModuleName :: SourceCode -> Maybe Text
extractModuleName = go . fmap snd . coerce @_ @[CodeLine]
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? ix 1) (match [re|^module\s+(\S+)|] x)

