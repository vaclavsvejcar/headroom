{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.Command
  ( commandParser
  )
where

import           Headroom.Command.Readers       ( licenseReader
                                                , licenseTypeReader
                                                )
import           Headroom.Meta                  ( buildVer )
import           Headroom.Types                 ( Command(..)
                                                , LicenseType
                                                )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
import           Options.Applicative
import           RIO
import qualified RIO.Text                      as T


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
  commands   = subparser (runCommand <> genCommand <> initCommand)
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
  initCommand = command
    "init"
    (info (initOptions <**> helper)
          (progDesc "initialize current project for Headroom")
    )

runOptions :: Parser Command
runOptions = undefined

genOptions :: Parser Command
genOptions =
  Gen
    <$> switch
          (long "config-file" <> short 'c' <> help
            "generate stub YAML config file to stdout"
          )
    <*> optional
          (option
            licenseReader
            (  long "license"
            <> short 'l'
            <> metavar "licenseType:fileType"
            <> help "generate template for license and file type"
            )
          )

initOptions :: Parser Command
initOptions =
  Init
    <$> option
          licenseTypeReader
          (long "license-type" <> short 'l' <> metavar "TYPE" <> help
            (  "type of open source license, available options: "
            <> T.unpack (allValuesToText @LicenseType)
            )
          )
    <*> some
          (strOption
            (long "source-path" <> short 's' <> metavar "PATH" <> help
              "path to source code file/directory"
            )
          )
