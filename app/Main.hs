{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Headroom.Command               ( commandParser )
import           Headroom.Command.Gen           ( commandGen
                                                , parseGenMode
                                                )
import           Headroom.Command.Init          ( commandInit )
import           Headroom.Types                 ( ApplicationError(..)
                                                , Command(..)
                                                , CommandGenOptions(..)
                                                , CommandInitOptions(..)
                                                )
import           Options.Applicative
import           Prelude                        ( putStrLn )
import           RIO

main :: IO ()
main = do
  command' <- execParser commandParser
  catch
    (bootstrap command')
    (\ex -> do
      putStrLn $ "ERROR: " <> displayException (ex :: ApplicationError)
      exitWith $ ExitFailure 1
    )

bootstrap :: Command -> IO ()
bootstrap = \case
  c@(Gen _ _) -> do
    genMode <- parseGenMode c
    commandGen (CommandGenOptions genMode)
  Init licenseType sourcePaths ->
    commandInit (CommandInitOptions sourcePaths licenseType)
  _ -> undefined
