{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Gen
  ( commandGen
  )
where

import           Headroom.Embedded              ( configFileStub )
import           Headroom.Command.Gen.Env
import           Headroom.Command.Shared        ( bootstrap )
import           RIO

env' :: GenOptions -> LogFunc -> IO Env
env' opts logFunc = return $ Env { envLogFunc = logFunc, envGenOptions = opts }

commandGen :: GenOptions -> IO ()
commandGen opts = bootstrap (env' opts) (goDebug opts) $ case goGenMode opts of
  GenConfigFile -> generateConfigFile

generateConfigFile :: HasLogFunc env => RIO env ()
generateConfigFile = logInfo configFileStub
