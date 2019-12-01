{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Gen.Env
  ( GenOptions(..)
  , Env(..)
  )
where

import           RIO

data GenOptions =
  GenOptions { goConfigFile :: Maybe FilePath
             , goDebug      :: Bool
             } deriving (Show)

data Env =
  Env { envLogFunc    :: !LogFunc
      , envGenOptions :: !GenOptions
      }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
