{-|
Module      : Headroom.Types.Utils
Description : tilities related to data types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Utilities related to data types.
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


-- | Returns all values of enum.
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

-- | Custom /Aeson/ options.
customOptions :: Options
customOptions =
  defaultOptions { fieldLabelModifier = symbolCase '-' . dropFieldPrefix }

-- | Drops prefix from camel-case text.
--
-- >>> dropFieldPrefix "xxHelloWorld"
-- "helloWorld"
dropFieldPrefix :: String -> String
dropFieldPrefix (x : n : xs) | C.isUpper x && C.isUpper n = x : n : xs
dropFieldPrefix (x : n : xs) | C.isUpper x = C.toLower x : n : xs
dropFieldPrefix (_ : xs)                   = dropFieldPrefix xs
dropFieldPrefix []                         = []

-- | Parses enum value from its string representation.
readEnumCI :: (Bounded a, Enum a, Show a) => ReadS a
readEnumCI str =
  let textRepr = fmap C.toLower . show
      result   = L.find (\item -> textRepr item == fmap C.toLower str) allValues
  in  maybe [] (\item -> [(item, "")]) result

-- | Transforms camel-case text into text cased with given symbol.
--
-- >>> symbolCase '-' "fooBar"
-- "foo-bar"
symbolCase :: Char   -- ^ word separator symbol
           -> String -- ^ input text
           -> String -- ^ processed text
symbolCase sym = process
 where
  process [] = []
  process (x : xs) | C.isUpper x = sym : C.toLower x : process xs
                   | otherwise   = x : process xs
