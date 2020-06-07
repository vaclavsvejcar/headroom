{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}


module Headroom.HeaderFn.UpdateCopyright
  ( -- * Helper Functions
    updateYears
  )
where

import qualified Headroom.Data.TextExtra       as TE
import           Headroom.Regex                 ( re
                                                , replace
                                                )
import           Headroom.Types                 ( CurrentYear(..) )
import           RIO


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
  replaceYear curr | TE.read curr == year = tshow year
                   | otherwise            = mconcat [curr, "-", tshow year]
  replaceRange full fromY toY | TE.read toY == year = full
                              | otherwise = mconcat [fromY, "-", tshow year]
  processYear' _    (curr : _) = replaceYear curr
  processYear' full _          = full
  processRange' full (fromY : toY : _) = replaceRange full fromY toY
  processRange' full _                 = full
