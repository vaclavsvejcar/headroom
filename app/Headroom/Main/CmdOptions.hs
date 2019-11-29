{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Headroom.Main.CmdOptions
  ( CmdOptions(..)
  , cmdOptions
  , toAppConfig
  )
where

import           Headroom.Main.Meta             ( buildVer )
import           Headroom.Main.OrphanInstances  ( )
import           Headroom.Types                 ( AppConfig(..) )
import qualified Data.Default                  as D
import           RIO
import qualified RIO.Text                      as T
import           System.Console.CmdArgs

data CmdOptions =
    Run { source_path     :: [FilePath]
        , template_path   :: [FilePath]
        , replace_headers :: Bool
        , debug           :: Bool
        }
  | Generate { bar :: T.Text }
  deriving (Eq, Data, Show, Typeable)

modeRun :: CmdOptions
modeRun =
  Run
      { source_path     = def &= typ "FILE/DIR" &= help
                            "path to source code file/directory"
      , template_path   = def &= typ "FILE/DIR" &= help
                            "path to header template file/directory"
      , replace_headers = False &= help "force replace existing headers"
      , debug           = False &= help "produce more verbose output"
      }
    &= help "add or replace source code headers"

modeGenerate :: CmdOptions
modeGenerate =
  Generate { bar = def &= help "test bar arg" } &= help "generator help text"

cmdOptions :: Mode (CmdArgs CmdOptions)
cmdOptions =
  cmdArgsMode
    $  modes [modeRun, modeGenerate]
    &= help "manage your source code license headers"
    &= program "headroom"
    &= summary
         (  "headroom v"
         ++ buildVer
         ++ " - https://github.com/vaclavsvejcar/headroom"
         )

toAppConfig :: CmdOptions -> AppConfig
toAppConfig (Run sourcePaths templatePaths replaceHeaders _) = D.def
  { acSourcePaths    = sourcePaths
  , acTemplatePaths  = templatePaths
  , acReplaceHeaders = replaceHeaders
  }
toAppConfig _ = D.def
