{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Gen
  ( runGenMode
  )
where

import           Headroom.Embedded              ( configFileStub )
import           Headroom.Gen.Env
import           RIO

runGenMode :: GenOptions -> IO ()
runGenMode opts = bootstrap opts $ case goGenMode opts of
  GenConfigFile -> generateConfigFile

bootstrap :: GenOptions -> RIO Env a -> IO a
bootstrap opts logic = do
  logOptions <- logOptionsHandle stderr (goDebug opts)
  let logOptions' = setLogUseLoc False logOptions
  withLogFunc logOptions' $ \logFunc -> do
    let env = Env { envLogFunc = logFunc, envGenOptions = opts }
    runRIO env logic

generateConfigFile :: HasLogFunc env => RIO env ()
generateConfigFile = logInfo configFileStub
