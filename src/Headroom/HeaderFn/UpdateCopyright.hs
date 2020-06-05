{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Headroom.HeaderFn.UpdateCopyright
  ( -- * Helper Functions
    updateYears
  )
where

import           Headroom.Regex                 ( re' )
import           Headroom.Types                 ( CurrentYear(..) )
import           RIO
import           RIO.Partial                    ( read )
import           Text.Regex.PCRE.Heavy          ( gsub )


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
