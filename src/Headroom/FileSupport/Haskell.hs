{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Headroom.FileSupport.Haskell
  ( extractModuleName
  , extractVariablesHaskell
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )
import           Headroom.FileSupport.Haskell.Haddock
                                                ( HaddockModuleHeader(..)
                                                , extractModuleHeader
                                                )
import           Headroom.Regex                 ( compile'
                                                , match'
                                                )
import           Headroom.Types                 ( HeaderConfig(..) )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Light          ( Regex )


-- | Extracts name of /Haskell/ module from given source code file content.
--
-- >>> extractModuleName "{-# LANGUAGE OverloadedStrings #-}\nmodule Foo where"
-- Just "Foo"
extractModuleName :: Text
                  -- ^ input text
                  -> Maybe Text
                  -- ^ extracted module name
extractModuleName = go . T.lines
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? element 1) (match' moduleNameRegex x)


-- | Extracts variables from /Haskell/ source code.
--
-- __List of Extracted Variables:__
--
-- * @___haskell_module_name__@ - name of the /Haskell/ module
-- * @___haskell_module_longdesc__@ - long description of /Haddock/ module
-- * @___haskell_module_shortdesc__@ - @Description@ field of /Haddock/ module header
extractVariablesHaskell :: HeaderConfig
                        -- ^ license header configuration
                        -> Maybe (Int, Int)
                        -- ^ license header position @(startLine, endLine)@
                        -> Text
                        -- ^ input text
                        -> HashMap Text Text
                        -- ^ extracted variables
extractVariablesHaskell _ headerPos text = HM.fromList
  [ ( "_haskell_module_name"
    , fromMaybe ">> NAME OF HASKELL MODULE <<" $ extractModuleName text
    )
  , ( "_haskell_module_longdesc"
    , fromMaybe ">> MODULE LONG DESCRIPTION <<" hmhLongDesc
    )
  , ( "_haskell_module_shortdesc"
    , fromMaybe ">> MODULE SHORT DESCRIPTION <<" hmhShortDesc
    )
  ]
 where
  HaddockModuleHeader {..} = extractModuleHeader headerText
  headerText               = maybe "" (\(s, e) -> cut s e text) headerPos
  cut s e = T.unlines . L.take (e - s) . L.drop s . T.lines

--------------------------------------------------------------------------------


moduleNameRegex :: Regex
moduleNameRegex = compile' "^module\\s+(\\S+)"
