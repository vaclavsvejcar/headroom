{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Headroom.Types.EnumExtra where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T

class (Bounded a, Enum a, Eq a, Ord a, Show a) => EnumExtra a where

  allValues :: [a]
  allValues = [minBound ..]

  allValuesToText :: Text
  allValuesToText = T.intercalate ", " (fmap enumToText (allValues :: [a]))

  enumToText :: a -> Text
  enumToText = T.pack . show

  textToEnum :: Text -> Maybe a
  textToEnum text =
    let enumValue v = (T.toLower . enumToText $ v) == T.toLower text
    in  L.find enumValue allValues
