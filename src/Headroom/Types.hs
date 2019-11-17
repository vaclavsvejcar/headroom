{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( AppConfig(..)
  , FileType(..)
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , genericParseJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Headroom.Types.Util            ( aesonOptions
                                                , allValues
                                                )
import           RIO
import qualified RIO.Char                      as C
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
    let textRepr   = fmap C.toLower . show
        maybeValue = L.find (\ft -> textRepr ft == s) (allValues :: [FileType])
    in  maybe [] (\x -> [(x, "")]) maybeValue
