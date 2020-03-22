{-|
Module      : Headroom.Command.Init.Env
Description : Environment for the Init command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and instances for the /Init/ command environment.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Init.Env where

import           Headroom.License               ( LicenseType )
import           RIO


-- | /RIO/ Environment for the /Init/ command.
data Env = Env
  { envLogFunc     :: !LogFunc
  , envInitOptions :: !InitOptions
  , envPaths       :: !Paths
  }

-- | Options for the /Init/ command.
data InitOptions = InitOptions
  { ioSourcePaths :: ![FilePath]
  , ioLicenseType :: !LicenseType
  }
  deriving Show

-- | Paths to various locations of file system.
data Paths = Paths
  { pCurrentDir   :: !FilePath
  , pConfigFile   :: !FilePath
  , pTemplatesDir :: !FilePath
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

-- | Environment value with /Init/ command options.
class HasInitOptions env where
  initOptionsL :: Lens' env InitOptions

-- | Environment value with 'Paths'.
class HasPaths env where
  pathsL :: Lens' env Paths

instance HasInitOptions Env where
  initOptionsL = lens envInitOptions (\x y -> x { envInitOptions = y })

instance HasPaths Env where
  pathsL = lens envPaths (\x y -> x { envPaths = y })
