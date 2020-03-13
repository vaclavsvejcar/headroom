{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Init.Env where

import           RIO

newtype InitOptions = InitOptions
  { ioSourcePaths :: [FilePath]
  } deriving Show

data Env = Env
  { envLogFunc     :: !LogFunc
  , envInitOptions :: InitOptions
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
