{-# LANGUAGE DeriveGeneric #-}
module Headroom.Types
  ( dropFieldPrefix
  )
where

import           Data.Aeson                     ( defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , FromJSON(parseJSON)
                                                , Options
                                                )
import qualified Data.Char                     as C
import           GHC.Generics                   ( Generic )

data AppConfig =
    AppConfig { acFoo :: String
              , acBar :: String
              } deriving (Generic, Show)

instance FromJSON AppConfig where
  parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix }

dropFieldPrefix :: String -> String
dropFieldPrefix (x : n : xs) | C.isUpper x && C.isUpper n = x : n : xs
dropFieldPrefix (x : n : xs) | C.isUpper x = C.toLower x : n : xs
dropFieldPrefix (_ : xs)                   = dropFieldPrefix xs
dropFieldPrefix []                         = []
