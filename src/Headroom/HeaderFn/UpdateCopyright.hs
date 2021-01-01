{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Headroom.HeaderFn.UpdateCopyright
Description : /License Header function/ for updating years in copyrights
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functionality for updating years in copyright statements
in already rendered /license headers/.
-}

module Headroom.HeaderFn.UpdateCopyright
  ( -- * Data Types
    SelectedAuthors(..)
  , UpdateCopyrightMode(..)
    -- * Header Functions
  , updateCopyright
    -- * Helper Functions
  , updateYears
  )
where

import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Regex                 ( re
                                                     , replace
                                                     )
import           Headroom.Data.TextExtra             ( mapLines
                                                     , read
                                                     )
import           Headroom.HeaderFn.Types             ( HeaderFn(..) )
import           Headroom.Types                      ( CurrentYear(..) )
import           RIO
import qualified RIO.NonEmpty                       as NE
import qualified RIO.Text                           as T


---------------------------------  DATA TYPES  ---------------------------------


-- | Non-empty list of authors for which to update years in their copyrights.
newtype SelectedAuthors = SelectedAuthors (NonEmpty Text) deriving (Eq, Show)


-- | Mode that changes behaviour of the 'updateCopyright' function.
data UpdateCopyrightMode
  = UpdateAllAuthors
  -- ^ updates years in copyrights for all authors
  | UpdateSelectedAuthors SelectedAuthors
  -- ^ updates years in copyrights only for selected authors
  deriving (Eq, Show)


------------------------------  PUBLIC FUNCTIONS  ------------------------------


-- | /License header function/ that updates years and year ranges in any
-- present copyright statements.
--
-- = Reader Environment Parameters
--   ['CurrentYear'] value of the current year
--   ['UpdateCopyrightMode'] mode specifying the behaviour of the updater
updateCopyright :: (Has CurrentYear env, Has UpdateCopyrightMode env)
                => HeaderFn env
updateCopyright = HeaderFn $ \input -> do
  currentYear <- viewL
  mode        <- viewL
  pure $ mapLines (update mode currentYear) input
 where
  update mode year line | shouldUpdate mode line = updateYears year line
                        | otherwise              = line
  shouldUpdate UpdateAllAuthors _ = True
  shouldUpdate (UpdateSelectedAuthors (SelectedAuthors authors)) input =
    any (`T.isInfixOf` input) (NE.toList authors)


-- | Updates years and years ranges in given text.
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2020"
-- "Copyright (c) 2020"
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2019"
-- "Copyright (c) 2019-2020"
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2018-2020"
-- "Copyright (c) 2018-2020"
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2018-2019"
-- "Copyright (c) 2018-2020"
updateYears :: CurrentYear
            -- ^ current year
            -> Text
            -- ^ text to update
            -> Text
            -- ^ text with updated years
updateYears (CurrentYear year) = processYear . processRange
 where
  processYear  = replace [re|(?!\d{4}-)(?<!-)(\d{4})|] processYear'
  processRange = replace [re|(\d{4})-(\d{4})|] processRange'
  replaceYear curr | read curr == Just year = tshow year
                   | otherwise              = mconcat [curr, "-", tshow year]
  replaceRange full fY tY | read tY == Just year = full
                          | otherwise            = mconcat [fY, "-", tshow year]
  processYear' _    (curr : _) = replaceYear curr
  processYear' full _          = full
  processRange' full (fromY : toY : _) = replaceRange full fromY toY
  processRange' full _                 = full
