{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Main
Description : Main application launcher
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Code responsible for booting up the application and parsing command line
arguments.
-}

module Main where

import           Headroom.Command               ( commandParser )
import           Headroom.Command.Gen           ( commandGen
                                                , parseGenMode
                                                )
import           Headroom.Command.Init          ( commandInit )
import           Headroom.Command.Run           ( commandRun )
import           Headroom.Types                 ( Command(..)
                                                , CommandGenOptions(..)
                                                , CommandInitOptions(..)
                                                , CommandRunOptions(..)
                                                )
import           Options.Applicative
import           RIO
import           System.IO                      ( hPutStrLn )


main :: IO ()
main = do
  command' <- execParser commandParser
  catch
    (bootstrap command')
    (\ex -> do
      hPutStrLn stderr $ "ERROR: " <> displayException (ex :: SomeException)
      exitWith $ ExitFailure 1
    )

bootstrap :: Command -> IO ()
bootstrap = \case
  c@(Gen _ _) -> do
    cgoGenMode <- parseGenMode c
    commandGen CommandGenOptions { .. }
  Init cioLicenseType cioSourcePaths -> commandInit CommandInitOptions { .. }
  Run croSourcePaths croExcludedPaths croTemplateSource croVariables croRunMode croDebug croDryRun
    -> commandRun CommandRunOptions { .. }
