{-|
Module      : Headroom.Command.Gen.Env
Description : Environment for the Generate command
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and instances for the @gen@ command environment.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Gen.Env
  ( GenMode(..)
  , GenOptions(..)
  , Env(..)
  )
where

import           RIO
import           RIO.Text                       ( Text )

data GenOptions = GenOptions
  { goGenMode :: !GenMode
  , goDebug   :: !Bool
  }
  deriving Show

data Env = Env
  { envLogFunc    :: !LogFunc
  , envGenOptions :: !GenOptions
  }

data GenMode = GenConfigFile | GenLicense Text
  deriving (Eq, Show)

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
