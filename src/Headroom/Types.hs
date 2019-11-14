{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( AppConfig(..)
  , dropFieldPrefix
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Options
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                )
import qualified Data.Char                     as C
import           GHC.Generics                   ( Generic )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T

data AppConfig =
    AppConfig { acFoo     :: T.Text
              , acBar     :: T.Text
              , acOptions :: HM.HashMap T.Text T.Text
              } deriving (Eq, Generic, Show)

instance FromJSON AppConfig where
  parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix }

dropFieldPrefix :: String -> String
dropFieldPrefix (x : n : xs) | C.isUpper x && C.isUpper n = x : n : xs
dropFieldPrefix (x : n : xs) | C.isUpper x = C.toLower x : n : xs
dropFieldPrefix (_ : xs)                   = dropFieldPrefix xs
dropFieldPrefix []                         = []
