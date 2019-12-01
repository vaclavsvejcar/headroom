{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Types
  ( AppConfig(..)
  , FileType(..)
  , Header(..)
  , HeadroomError(..)
  , NewLine(..)
  , Progress(..)
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , genericParseJSON
                                                )
import           Data.Default                   ( Default
                                                , def
                                                )
import           GHC.Generics                   ( Generic )
import           Headroom.Types.Util            ( customOptions
                                                , readEnumCI
                                                )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Read                      ( readsPrec )
import           Text.Printf                    ( printf )


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

data HeadroomError =
  InvalidPlaceholder T.Text
  | NoGenModeSelected
  deriving (Typeable)

data NewLine = CR | CRLF | LF deriving (Eq, Show)

data Progress = Progress Integer Integer deriving (Eq)

----------------------------  TYPE CLASS INSTANCES  ----------------------------

instance Exception HeadroomError

instance FromJSON AppConfig where
  parseJSON = genericParseJSON customOptions

instance Read FileType where
  readsPrec _ = readEnumCI

instance Default AppConfig where
  def = AppConfig 1 False [] [] HM.empty

instance Semigroup AppConfig where
  x <> y = AppConfig (acConfigVersion x `min` acConfigVersion y)
                     (acReplaceHeaders x)
                     (acSourcePaths x <> acSourcePaths y)
                     (acTemplatePaths x <> acTemplatePaths y)
                     (acPlaceholders x <> acPlaceholders y)

instance Monoid AppConfig where
  mempty = def

instance Show HeadroomError where
  show (InvalidPlaceholder raw) =
    "Cannot parse placeholder key=value from: " <> T.unpack raw
  show NoGenModeSelected
    = "Please select at least one option what to generate (see --help for details)"

instance Show Progress where
  show (Progress current total) = "[" <> currentS <> " of " <> totalS <> "]"
   where
    format   = "%" <> (show . L.length $ totalS) <> "d"
    currentS = printf format current
    totalS   = show total
