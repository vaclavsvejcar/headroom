{-|
Module      : Headroom.Command
Description : Command line options processing
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and functions for parsing command line options.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Command
  ( Command(..)
  , commandParser
  )
where

import           Data.Semigroup                 ( (<>) )
import           Headroom.Meta                  ( buildVer )
import           Headroom.Types                 ( RunMode(..) )
import           Options.Applicative
import           RIO
import           RIO.Text                       ( Text )

-- | Application command.
data Command
  = Run [FilePath] [FilePath] [Text] RunMode Bool -- ^ /Run/ command
  | Gen Bool (Maybe Text)                         -- ^ /Generator/ command
    deriving (Show)

-- | Parses command line arguments.
commandParser :: ParserInfo Command
commandParser = info
  (commands <**> helper)
  (  fullDesc
  <> progDesc "manage your source code license headers"
  <> header header'
  )
 where
  header' =
    "headroom v" <> buildVer <> " :: https://github.com/vaclavsvejcar/headroom"
  commands   = subparser (runCommand <> genCommand)
  runCommand = command
    "run"
    (info (runOptions <**> helper)
          (progDesc "add or replace source code headers")
    )
  genCommand = command
    "gen"
    (info (genOptions <**> helper)
          (progDesc "generate stub configuration and template files")
    )


runOptions :: Parser Command
runOptions =
  Run
    <$> many
          (strOption
            (long "source-path" <> short 's' <> metavar "PATH" <> help
              "path to source code file/directory"
            )
          )
    <*> many
          (strOption
            (long "template-path" <> short 't' <> metavar "PATH" <> help
              "path to header template file/directory"
            )
          )
    <*> many
          (strOption
            (long "variables" <> short 'v' <> metavar "KEY=VALUE" <> help
              "values for template variables"
            )
          )
    <*> (   flag'
            Replace
            (long "replace-headers" <> short 'r' <> help
              "force replace existing license headers"
            )
        <|> flag'
              Drop
              (long "drop-headers" <> short 'd' <> help
                "drop existing license headers only"
              )
        <|> pure Add
        )
    <*> switch (long "debug" <> help "produce more verbose output")

genOptions :: Parser Command
genOptions =
  Gen
    <$> switch
          (long "config-file" <> short 'c' <> help
            "generate stub YAML config file to stdout"
          )
    <*> optional
          (strOption
            (long "license" <> short 'l' <> metavar "name:type" <> help
              "generate template for license and file type"
            )
          )
