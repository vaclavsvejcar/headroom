{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types.Util
  ( aesonOptions
  , allValues
  , dropFieldPrefix
  , readEnumCI
  )
where

import           Data.Aeson                     ( Options
                                                , defaultOptions
                                                , fieldLabelModifier
                                                )
import           RIO
import qualified RIO.Char                      as C
import qualified RIO.List                      as L
import           Text.Read                      ( ReadS )


aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix }

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

dropFieldPrefix :: String -> String
dropFieldPrefix (x : n : xs) | C.isUpper x && C.isUpper n = x : n : xs
dropFieldPrefix (x : n : xs) | C.isUpper x = C.toLower x : n : xs
dropFieldPrefix (_ : xs)                   = dropFieldPrefix xs
dropFieldPrefix []                         = []

readEnumCI :: (Bounded a, Enum a, Show a) => ReadS a
readEnumCI str =
  let textRepr = fmap C.toLower . show
      result   = L.find (\item -> textRepr item == fmap C.toLower str) allValues
  in  maybe [] (\item -> [(item, "")]) result
