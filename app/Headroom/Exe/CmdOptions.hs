{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Headroom.Exe.CmdOptions
  ( CmdOptions(..)
  , cmdOptions
  )
where

import           Headroom.Exe.OrphanInstances   ( )
import           RIO
import qualified RIO.Text                      as T
import           System.Console.CmdArgs

data CmdOptions =
    Run { foo :: T.Text }
  | Generate { bar :: T.Text }
  deriving (Eq, Data, Show, Typeable)

modeRun :: CmdOptions
modeRun = Run { foo = def &= help "test foo arg" } &= help "run help text"

modeTest :: CmdOptions
modeTest =
  Generate { bar = def &= help "test bar arg" } &= help "generator help text"

cmdOptions :: Mode (CmdArgs CmdOptions)
cmdOptions =
  cmdArgsMode
    $  modes [modeRun, modeTest]
    &= help "summary text here"
    &= program "headroom"
    &= summary "headroom v1.0"
