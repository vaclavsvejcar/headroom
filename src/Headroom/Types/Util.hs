{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types.Util
  ( aesonOptions
  , allValues
  , dropFieldPrefix
  )
where

import           Data.Aeson                     ( Options
                                                , defaultOptions
                                                , fieldLabelModifier
                                                )
import           RIO
import qualified RIO.Char                      as C


aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix }

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

dropFieldPrefix :: String -> String
dropFieldPrefix (x : n : xs) | C.isUpper x && C.isUpper n = x : n : xs
dropFieldPrefix (x : n : xs) | C.isUpper x = C.toLower x : n : xs
dropFieldPrefix (_ : xs)                   = dropFieldPrefix xs
dropFieldPrefix []                         = []
