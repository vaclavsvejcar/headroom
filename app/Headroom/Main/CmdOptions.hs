{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Headroom.Main.CmdOptions
  ( CmdOptions(..)
  , cmdOptions
  )
where

import           Headroom.Main.OrphanInstances  ( )
import           RIO
import qualified RIO.Text                      as T
import           System.Console.CmdArgs

data CmdOptions =
    Run { source_path :: [FilePath]
        , template_path :: [FilePath]
        , replace_headers :: Bool
        }
  | Generate { bar :: T.Text }
  deriving (Eq, Data, Show, Typeable)

modeRun :: CmdOptions
modeRun =
  Run { source_path     = def &= typ "FILE/DIR" &= help "test foo arg"
      , template_path   = def &= typ "FILE/DIR" &= help "foo"
      , replace_headers = False &= help "foo bar"
      }
    &= help "run help text"

modeGenerate :: CmdOptions
modeGenerate =
  Generate { bar = def &= help "test bar arg" } &= help "generator help text"

cmdOptions :: Mode (CmdArgs CmdOptions)
cmdOptions =
  cmdArgsMode
    $  modes [modeRun, modeGenerate]
    &= help "summary text here"
    &= program "headroom"
    &= summary "headroom v1.0"
