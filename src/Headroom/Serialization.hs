{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Serialization
  ( aesonOptions
  , dropFieldPrefix
  , prettyPrintYAML
  , symbolCase
  )
where

import           Data.Aeson                     ( Options
                                                , ToJSON(..)
                                                , defaultOptions
                                                , fieldLabelModifier
                                                )
import qualified Data.Yaml.Pretty              as YP
import           RIO
import qualified RIO.Char                      as C



-- | Custom /Aeson/ options.
aesonOptions :: Options
aesonOptions =
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

prettyPrintYAML :: ToJSON a => a -> Text
prettyPrintYAML = decodeUtf8Lenient . YP.encodePretty prettyConfig
  where prettyConfig = YP.setConfCompare compare YP.defConfig

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
