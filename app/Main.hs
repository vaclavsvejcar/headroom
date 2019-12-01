{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Headroom.Main.CmdOptions       ( CmdOptions(Gen, Run)
                                                , cmdOptions
                                                )
import           Headroom.Gen                   ( runGenMode )
import           Headroom.Gen.Env               ( GenMode(..)
                                                , GenOptions(GenOptions)
                                                )
import           Headroom.Run                   ( runRunMode )
import           Headroom.Run.Env               ( RunOptions(RunOptions) )
import           Headroom.Types                 ( HeadroomError(..) )
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = cmdArgsRun cmdOptions >>= selectMode
 where
  selectMode (Run sourcePaths templatePaths replaceHeaders placeholders debug)
    = runRunMode
      (RunOptions replaceHeaders sourcePaths templatePaths placeholders debug)
  selectMode g@(Gen _ debug) = do
    genMode <- parseGenMode g
    runGenMode (GenOptions genMode debug)

parseGenMode :: MonadThrow m => CmdOptions -> m GenMode
parseGenMode (Gen True _) = return GenConfigFile
parseGenMode _            = throwM NoGenModeSelected
