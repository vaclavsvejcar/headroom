{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Gen.Env
  ( GenMode(..)
  , GenOptions(..)
  , Env(..)
  )
where

import           RIO

data GenOptions =
  GenOptions { goGenMode :: !GenMode
             , goDebug   :: !Bool
             } deriving (Show)

data Env =
  Env { envLogFunc    :: !LogFunc
      , envGenOptions :: !GenOptions
      }

data GenMode = GenConfigFile deriving (Bounded, Enum, Eq, Ord, Show)

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
