{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileSupport.Haskell
  ( extractModuleName
  , extractVariablesHaskell
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )
import           Headroom.Regex                 ( compile'
                                                , match'
                                                )
import           Headroom.Types                 ( HeaderConfig(..) )
import           RIO
import qualified RIO.HashMap                   as HM
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
extractVariablesHaskell :: HeaderConfig
                        -- ^ license header configuration
                        -> Maybe (Int, Int)
                        -- ^ license header position @(startLine, endLine)@
                        -> Text
                        -- ^ input text
                        -> HashMap Text Text
                        -- ^ extracted variables
extractVariablesHaskell _ _ text = HM.fromList
  [ ( "_haskell_module_name"
    , fromMaybe ">> NAME OF HASKELL MODULE <<" $ extractModuleName text
    )
  ]

--------------------------------------------------------------------------------


moduleNameRegex :: Regex
moduleNameRegex = compile' "^module\\s+(\\S+)"
