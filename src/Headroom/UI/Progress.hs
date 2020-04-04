{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.UI.Progress
  ( Progress(..)
  , zipWithProgress
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Printf                    ( printf )


-- | Progress indication. First argument is current progress, second the maximum
-- value.
data Progress = Progress Int Int
  deriving (Eq, Show)

instance Display Progress where
  textDisplay (Progress current total) = T.pack
    $ mconcat ["[", currentS, " of ", totalS, "]"]
   where
    format   = "%" <> (show . L.length $ totalS) <> "d"
    currentS = printf format current
    totalS   = show total

-- | Zips given list with the progress info.
--
-- >>> zipWithProgress ["a", "b"]
-- [(Progress 1 2,"a"),(Progress 2 2,"b")]
zipWithProgress :: [a] -> [(Progress, a)]
zipWithProgress list = zip progresses list
 where
  listLength = L.length list
  progresses = fmap (`Progress` listLength) [1 .. listLength]
