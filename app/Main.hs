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
{-# LANGUAGE LambdaCase        #-}
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
import           Headroom.Command.Gen.Errors    ( GenCommandError(..) )
import           Headroom.Command.Init          ( commandInit )
import           Headroom.Command.Init.Env      ( InitOptions(InitOptions) )
import           Headroom.Command.Init.Errors   ( InitCommandError(..) )

import           Headroom.Command.Run           ( commandRun )
import           Headroom.Command.Run.Env       ( RunOptions(RunOptions) )
import           Headroom.License               ( LicenseType
                                                , parseLicenseType
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
bootstrap = \case
  Run sourcePaths templatePaths variables runMode debug ->
    commandRun (RunOptions runMode sourcePaths templatePaths variables debug)
  c@(Gen _ _) -> do
    genMode <- parseGenMode c
    commandGen (GenOptions genMode)
  Init licenseType sourcePaths -> do
    licenseType' <- parseLicenseType' licenseType
    commandInit (InitOptions sourcePaths licenseType')

parseGenMode :: MonadThrow m => Command -> m GenMode
parseGenMode = \case
  Gen True  Nothing        -> pure GenConfigFile
  Gen False (Just license) -> pure $ GenLicense license
  _                        -> throwM $ GenCommandError NoGenModeSelected

parseLicenseType' :: MonadThrow m => Text -> m LicenseType
parseLicenseType' input = case parseLicenseType input of
  Just licenseType -> pure licenseType
  Nothing          -> throwM $ InitCommandError (InvalidLicenseType input)
