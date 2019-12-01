{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# OPTIONS_GHC -fno-cse            #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Headroom.Main.CmdOptions
  ( CmdOptions(..)
  , cmdOptions
  )
where

import           Headroom.Main.Meta             ( buildVer )
import           Headroom.Main.OrphanInstances  ( )
import           RIO
import qualified RIO.Text                      as T
import           System.Console.CmdArgs

data CmdOptions =
    Run { source_path     :: [FilePath]
        , template_path   :: [FilePath]
        , replace_headers :: Bool
        , placeholder     :: [T.Text]
        , debug           :: Bool
        }
  | Gen { config_file :: Bool
        , debug       :: Bool
        }
  deriving (Eq, Data, Show, Typeable)

modeRun :: CmdOptions
modeRun =
  Run
      { source_path     = def &= typ "FILE/DIR" &= help
                            "path to source code file/directory"
      , template_path   = def &= typ "FILE/DIR" &= help
                            "path to header template file/directory"
      , replace_headers = False &= help "force replace existing headers"
      , placeholder     = def &= typ "KEY=VALUE" &= help
                            "placeholder to replace in templates"
      , debug           = False &= help "produce more verbose output"
      }
    &= help "add or replace source code headers"

modeGen :: CmdOptions
modeGen =
  Gen { config_file = def &= help "generates stub YAML config file to stdout"
      , debug       = False &= help "produce more verbose output"
      }
    &= help "generate stub configuration and template files"

cmdOptions :: Mode (CmdArgs CmdOptions)
cmdOptions =
  cmdArgsMode
    $  modes [modeRun, modeGen]
    &= help "manage your source code license headers"
    &= program "headroom"
    &= summary
         (  "headroom v"
         ++ buildVer
         ++ " - https://github.com/vaclavsvejcar/headroom"
         )

