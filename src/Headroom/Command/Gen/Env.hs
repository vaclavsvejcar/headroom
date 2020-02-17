{-|
Module      : Headroom.Command.Gen.Env
Description : Environment for the Generate command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and instances for the /Generator/ command environment.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Gen.Env
  ( GenMode(..)
  , GenOptions(..)
  , Env(..)
  )
where

import           RIO


-- | Options for the /Generator/ command.
newtype GenOptions = GenOptions
  { goGenMode :: GenMode -- ^ used /Generator/ command mode
  }
  deriving Show

-- | /RIO/ Environment for the /Generator/ command.
data Env = Env
  { envLogFunc    :: !LogFunc    -- ^ logging function
  , envGenOptions :: !GenOptions -- ^ options
  }

-- | Represents what action should the /Generator/ perform.
data GenMode
  = GenConfigFile   -- ^ generate /YAML/ config file stub
  | GenLicense Text -- ^ generate license header template
  deriving (Eq, Show)

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
