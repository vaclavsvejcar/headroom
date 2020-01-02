{-|
Module      : Main
Description : Application entry point
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Functions responsible for application bootstrap.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Headroom.Command               ( Command(..)
                                                , commandParser
                                                )
import           Headroom.Command.Gen           ( commandGen )
import           Headroom.Command.Gen.Env       ( GenMode(..)
                                                , GenOptions(GenOptions)
                                                )
import           Headroom.Command.Run           ( commandRun )
import           Headroom.Command.Run.Env       ( RunOptions(RunOptions) )
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
  Run sourcePaths templatePaths placeholders runMode debug ->
    commandRun (RunOptions runMode sourcePaths templatePaths placeholders debug)
  c@(Gen _ _ debug) -> do
    genMode <- parseGenMode c
    commandGen (GenOptions genMode debug)

parseGenMode :: MonadThrow m => Command -> m GenMode
parseGenMode (Gen True  Nothing        _) = return GenConfigFile
parseGenMode (Gen False (Just license) _) = return $ GenLicense license
parseGenMode _                            = throwM NoGenModeSelected
