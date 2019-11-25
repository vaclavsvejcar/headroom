{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Main.App
  ( App(..)
  , HasCmdOptions(..)
  )
where

import           Headroom.Main.CmdOptions       ( CmdOptions )
import           RIO

data App =
  App { appCmdOptions :: !CmdOptions
      , appLogFunc :: !LogFunc
      }

class HasCmdOptions env where
  cmdOptionsL :: Lens' env CmdOptions

instance HasCmdOptions App where
  cmdOptionsL = lens appCmdOptions (\x y -> x { appCmdOptions = y })

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
