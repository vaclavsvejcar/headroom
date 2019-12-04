{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Headroom.Command.Gen           ( commandGen )
import           Headroom.Command.Gen.Env       ( GenMode(..)
                                                , GenOptions(GenOptions)
                                                )
import           Headroom.Command.Run           ( commandRun )
import           Headroom.Command.Run.Env       ( RunOptions(RunOptions) )
import           Headroom.Main.Options          ( Command(..)
                                                , commandParser
                                                )
import           Headroom.Types                 ( HeadroomError(..) )
import           Options.Applicative
import           Prelude                        ( putStrLn )
import           RIO

main :: IO ()
main = do
  command' <- execParser commandParser
  catch
    (bootstrap command')
    (\ex -> do
      putStrLn $ "ERROR: " <> displayException (ex :: HeadroomError)
      exitWith $ ExitFailure 1
    )

bootstrap :: Command -> IO ()
bootstrap command' = case command' of
  Run sourcePaths templatePaths placeholders replaceHeaders debug -> commandRun
    (RunOptions replaceHeaders sourcePaths templatePaths placeholders debug)
  c@(Gen _ debug) -> do
    genMode <- parseGenMode c
    commandGen (GenOptions genMode debug)

parseGenMode :: MonadThrow m => Command -> m GenMode
parseGenMode (Gen True _) = return GenConfigFile
parseGenMode _            = throwM NoGenModeSelected
