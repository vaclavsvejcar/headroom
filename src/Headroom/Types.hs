{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( AppConfig(..)
  , FileType(..)
  , Header(..)
  , NewLine(..)
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , genericParseJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Headroom.Types.Util            ( customOptions
                                                , readEnumCI
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import           Text.Read                      ( readsPrec )


data AppConfig =
  AppConfig { acConfigVersion  :: Int
            , acReplaceHeaders :: Bool
            , acSourcePaths    :: [FilePath]
            , acTemplatePaths  :: [FilePath]
            , acPlaceholders   :: HM.HashMap T.Text T.Text
            } deriving (Eq, Generic, Show)

data FileType = Haskell deriving (Bounded, Enum, Eq, Ord, Show)

data Header =
  Header { hFileType :: FileType
         , hContent  :: T.Text
         } deriving (Eq, Show)

data NewLine = CR | CRLF | LF deriving (Eq, Show)

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance FromJSON AppConfig where
  parseJSON = genericParseJSON customOptions

instance Read FileType where
  readsPrec _ = readEnumCI
