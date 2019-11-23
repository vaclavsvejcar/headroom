{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Headroom.Main.OrphanInstances where

import qualified RIO.Text                      as T
import qualified System.Console.CmdArgs        as CmdArgs

instance CmdArgs.Default T.Text where
  def = T.empty
