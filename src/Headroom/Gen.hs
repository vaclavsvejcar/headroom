{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Gen
  ( genMode
  )
where

import           Control.Monad.Extra            ( whenJust )
import           Headroom.Gen.Env
import           RIO

genMode :: GenOptions -> IO ()
genMode opts = bootstrap opts $ do
  whenJust (goConfigFile opts) generateConfigFile

bootstrap :: GenOptions -> RIO Env a -> IO a
bootstrap opts logic = do
  logOptions <- logOptionsHandle stderr (goDebug opts)
  let logOptions' = setLogUseLoc False logOptions
  withLogFunc logOptions' $ \logFunc -> do
    let env = Env { envLogFunc = logFunc, envGenOptions = opts }
    runRIO env logic

generateConfigFile :: HasLogFunc env => FilePath -> RIO env ()
generateConfigFile path = do
  logInfo "TODO generate config file"
