{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Headroom.Main.CmdOptions       ( CmdOptions(Gen, Run)
                                                , cmdOptions
                                                )
import           Headroom.Gen                   ( genMode )
import           Headroom.Gen.Env               ( GenOptions(GenOptions) )
import           Headroom.Run                   ( runMode )
import           Headroom.Run.Env               ( RunOptions(RunOptions) )
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = cmdArgsRun cmdOptions >>= selectMode
 where
  selectMode (Run sourcePaths templatePaths replaceHeaders placeholders debug)
    = runMode
      (RunOptions replaceHeaders sourcePaths templatePaths placeholders debug)
  selectMode (Gen configFile debug) = genMode (GenOptions configFile debug)
