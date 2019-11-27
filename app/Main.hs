{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Headroom.Main.CmdOptions       ( CmdOptions(Run)
                                                , cmdOptions
                                                )
import           Headroom.Run                   ( runMode )
import           Headroom.Run.Env               ( RunOptions(RunOptions) )
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = cmdArgsRun cmdOptions >>= selectMode
 where
  selectMode (Run sourcePaths templatePaths replaceHeaders) =
    runMode (RunOptions replaceHeaders sourcePaths templatePaths)
  selectMode _ = undefined  -- TODO generator mode
