{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Headroom.Main.CmdOptions       ( CmdOptions(Gen, Run)
                                                , cmdOptions
                                                )
import           Headroom.Gen                   ( runGenMode )
import           Headroom.Gen.Env               ( GenMode(..)
                                                , GenOptions(GenOptions)
                                                )
import           Headroom.Command.Run           ( commandRun )
import           Headroom.Command.Run.Env       ( RunOptions(RunOptions) )
import           Headroom.Types                 ( HeadroomError(..) )
import           Prelude                        ( putStrLn )
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = catch (cmdArgsRun cmdOptions >>= selectMode) wrapException
 where
  wrapException ex = do -- TODO handle this using RIO's logError
    putStrLn $ "ERROR: " <> displayException (ex :: SomeException)
    exitWith $ ExitFailure 1
  selectMode (Run sourcePaths templatePaths replaceHeaders placeholders debug)
    = commandRun
      (RunOptions replaceHeaders sourcePaths templatePaths placeholders debug)
  selectMode g@(Gen _ debug) = do
    genMode <- parseGenMode g
    runGenMode (GenOptions genMode debug)

parseGenMode :: MonadThrow m => CmdOptions -> m GenMode
parseGenMode (Gen True _) = return GenConfigFile
parseGenMode _            = throwM NoGenModeSelected
