{-|
Module      : Headroom.Types.Utils
Description : Shared data types utilities
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Shared functions and instances for data types.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types.Utils
  ( allValues
  , customOptions
  , dropFieldPrefix
  , readEnumCI
  , symbolCase
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


allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

customOptions :: Options
customOptions =
  defaultOptions { fieldLabelModifier = symbolCase '-' . dropFieldPrefix }

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

symbolCase :: Char -> String -> String
symbolCase sym = process
 where
  process [] = []
  process (x : xs) | C.isUpper x = sym : C.toLower x : process xs
                   | otherwise   = x : process xs
