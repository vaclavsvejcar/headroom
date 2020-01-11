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
  Run sourcePaths templatePaths variables runMode debug ->
    commandRun (RunOptions runMode sourcePaths templatePaths variables debug)
  c@(Gen _ _) -> do
    genMode <- parseGenMode c
    commandGen (GenOptions genMode)

parseGenMode :: MonadThrow m => Command -> m GenMode
parseGenMode (Gen True  Nothing       ) = return GenConfigFile
parseGenMode (Gen False (Just license)) = return $ GenLicense license
parseGenMode _                          = throwM NoGenModeSelected
