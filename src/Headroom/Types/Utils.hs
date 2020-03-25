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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Headroom.Types.Utils
  ( allValues
  , customOptions
  , dropFieldPrefix
  , readEnumCI
  , showEnumLC
  , showEnumValuesLC
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
dropFieldPrefix = \case
  (x : n : xs) | C.isUpper x && C.isUpper n -> x : n : xs
  (x : n : xs) | C.isUpper x -> C.toLower x : n : xs
  (_ : xs)                   -> dropFieldPrefix xs
  []                         -> []

-- | Shows all values of the specified enum, lowercase, comma separated.
--
-- >>> :set -XTypeApplications
-- >>> showEnumValuesLC @Bool
-- "false, true"
showEnumValuesLC :: forall a . (Bounded a, Enum a, Show a) => String
showEnumValuesLC = L.intercalate ", " values
  where values = fmap showEnumLC (allValues :: [a])

-- | Parses enum value from its string representation, case insensitive.
readEnumCI :: (Bounded a, Enum a, Show a) => ReadS a
readEnumCI str =
  let res = L.find (\item -> showEnumLC item == fmap C.toLower str) allValues
  in  maybe [] (\item -> [(item, "")]) res

-- | Shows enum value, lowercase.
--
-- >>> showEnumLC False
-- "false"
showEnumLC :: Show a => a -> String
showEnumLC = fmap C.toLower . show

-- | Transforms camel-case text into text cased with given symbol.
--
-- >>> symbolCase '-' "fooBar"
-- "foo-bar"
symbolCase :: Char   -- ^ word separator symbol
           -> String -- ^ input text
           -> String -- ^ processed text
symbolCase sym = \case
  [] -> []
  (x : xs) | C.isUpper x -> sym : C.toLower x : symbolCase sym xs
           | otherwise   -> x : symbolCase sym xs
