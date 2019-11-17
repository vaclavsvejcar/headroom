{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( AppConfig(..)
  , FileType(..)
  , allValues
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
import           RIO.Char                       ( toLower )
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Read                      ( readsPrec )

data AppConfig =
    AppConfig { acFoo     :: T.Text
              , acBar     :: T.Text
              , acOptions :: HM.HashMap T.Text T.Text
              } deriving (Eq, Generic, Show)

data FileType = CSS | Haskell | HTML deriving (Bounded, Enum, Eq, Ord, Show)

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance FromJSON AppConfig where
  parseJSON = genericParseJSON aesonOptions

instance Read FileType where
  readsPrec _ s =
    let textRepr   = fmap toLower . show
        maybeValue = L.find (\ft -> textRepr ft == s) (allValues :: [FileType])
    in  maybe [] (\x -> [(x, "")]) maybeValue

------------------------------  HELPER FUNCTIONS  ------------------------------

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix }

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

dropFieldPrefix :: String -> String
dropFieldPrefix (x : n : xs) | C.isUpper x && C.isUpper n = x : n : xs
dropFieldPrefix (x : n : xs) | C.isUpper x = C.toLower x : n : xs
dropFieldPrefix (_ : xs)                   = dropFieldPrefix xs
dropFieldPrefix []                         = []
