{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Init.Env where

import           Headroom.License               ( LicenseType )
import           RIO

data InitOptions = InitOptions
  { ioSourcePaths :: ![FilePath]
  , ioLicenseType :: !LicenseType
  }
  deriving Show

data Env = Env
  { envLogFunc     :: !LogFunc
  , envInitOptions :: !InitOptions
  , envPaths       :: !Paths
  }

data Paths = Paths
  { pCurrentDir   :: !FilePath
  , pConfigFile   :: !FilePath
  , pTemplatesDir :: !FilePath
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

class HasInitOptions env where
  initOptionsL :: Lens' env InitOptions

class HasPaths env where
  pathsL :: Lens' env Paths

instance HasInitOptions Env where
  initOptionsL = lens envInitOptions (\x y -> x { envInitOptions = y })

instance HasPaths Env where
  pathsL = lens envPaths (\x y -> x { envPaths = y })
