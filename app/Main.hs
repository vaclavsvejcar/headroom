{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Headroom.Command.Gen           ( commandGen )
import           Headroom.Command.Gen.Env       ( GenMode(..)
                                                , GenOptions(GenOptions)
                                                )
import           Headroom.Command.Run           ( commandRun )
import           Headroom.Command.Run.Env       ( RunOptions(RunOptions) )
import           Headroom.Main.CmdOptions       ( CmdOptions(Gen, Run)
                                                , cmdOptions
                                                )
import           Headroom.Types                 ( HeadroomError(..) )
import           Prelude                        ( putStrLn )
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = catch (cmdArgsRun cmdOptions >>= selectMode) wrapException
 where
  wrapException ex = do -- TODO handle this using RIO's logError
    putStrLn $ "ERROR: " <> displayException (ex :: HeadroomError)
    exitWith $ ExitFailure 1
  selectMode (Run sourcePaths templatePaths replaceHeaders placeholders debug)
    = commandRun
      (RunOptions replaceHeaders sourcePaths templatePaths placeholders debug)
  selectMode g@(Gen _ debug) = do
    genMode <- parseGenMode g
    commandGen (GenOptions genMode debug)

parseGenMode :: MonadThrow m => CmdOptions -> m GenMode
parseGenMode (Gen True _) = return GenConfigFile
parseGenMode _            = throwM NoGenModeSelected
