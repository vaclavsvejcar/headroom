{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


{-|
Module      : Headroom.FileSupport.Haskell
Description : Extraction of /Haskell/-specific template variables
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides support for extracting /Haskell/-specific info from the
parsed /source code files/ as /template variables/. Such info includes
/module name/ and selected fields from /Haddock module header/
(see "Headroom.FileSupport.Haskell.Haddock").
-}

module Headroom.FileSupport.Haskell
  ( -- * Variables Extraction
    extractModuleName
  , extractVariablesHaskell
    -- * Helper Functions
  , updateYears
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )
import           Headroom.FileSupport.Haskell.Haddock
                                                ( HaddockModuleHeader(..)
                                                , extractModuleHeader
                                                )
import           Headroom.Regex                 ( gsub
                                                , match'
                                                , re'
                                                )
import           Headroom.Types                 ( HeaderConfig(..)
                                                , Variables(..)
                                                )
import           Headroom.Variables             ( mkVariables )
import           RIO
import qualified RIO.List                      as L
import           RIO.Partial                    ( read )
import qualified RIO.Text                      as T



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
  go (x : xs) = maybe (go xs) (^? element 1) (match' [re'|^module\s+(\S+)|] x)


-- | Extracts variables from /Haskell/ source code.
--
-- __List of Extracted Variables:__
--
-- * @___haskell_module_copyright__@ - @Copyright@ field of /Haddock/ module header
-- * @___haskell_module_name__@ - name of the /Haskell/ module
-- * @___haskell_module_longdesc__@ - long description of /Haddock/ module
-- * @___haskell_module_shortdesc__@ - @Description@ field of /Haddock/ module header
extractVariablesHaskell :: HeaderConfig
                        -- ^ license header configuration
                        -> Maybe (Int, Int)
                        -- ^ license header position @(startLine, endLine)@
                        -> Text
                        -- ^ input text
                        -> Variables
                        -- ^ extracted variables
extractVariablesHaskell _ headerPos text = (mkVariables . catMaybes)
  [ ("_haskell_module_copyright", ) <$> hmhCopyright
  , ("_haskell_module_name", ) <$> extractModuleName text
  , ("_haskell_module_longdesc", ) <$> hmhLongDesc
  , ("_haskell_module_shortdesc", ) <$> hmhShortDesc
  ]
 where
  HaddockModuleHeader {..} = extractModuleHeader headerText
  headerText               = maybe "" (\(s, e) -> cut s e text) headerPos
  cut s e = T.unlines . L.take (e - s) . L.drop s . T.lines


-- | Updates years and years ranges in given text.
--
-- >>> updateYears 2020 "Copyright (c) 2020"
-- "Copyright (c) 2020"
--
-- >>> updateYears 2020 "Copyright (c) 2019"
-- "Copyright (c) 2019-2020"
--
-- >>> updateYears 2020 "Copyright (c) 2018-2020"
-- "Copyright (c) 2018-2020"
--
-- >>> updateYears 2020 "Copyright (c) 2018-2019"
-- "Copyright (c) 2018-2020"
updateYears :: Integer
            -- ^ current year
            -> Text
            -- ^ text to update
            -> Text
            -- ^ text with updated years
updateYears year = processYear . processRange
 where
  processYear  = gsub [re'|(?!\d{4}-)(?<!-)(\d{4})|] processYear'
  processRange = gsub [re'|(\d{4})-(\d{4})|] processRange'
  replaceYear curr | read curr == year = show year
                   | otherwise         = mconcat [curr, "-", show year]
  replaceRange full fromY toY | read toY == year = full
                              | otherwise = mconcat [fromY, "-", show year]
  processYear' _    (curr : _) = replaceYear curr
  processYear' full _          = full
  processRange' full (fromY : toY : _) = replaceRange full fromY toY
  processRange' full _                 = full
