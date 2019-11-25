{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Headroom.Main.App
import           Headroom.Main.CmdOptions       ( cmdOptions )
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = runApp bootstrap

bootstrap :: RIO App ()
bootstrap = do
  logInfo "starting app..."
  displayArgs

runApp :: RIO App a -> IO a
runApp inner = do
  cmdOptions' <- cmdArgsRun cmdOptions
  logOptions  <- logOptionsHandle stderr False
  withLogFunc logOptions $ \logFunc -> do
    let app = App { appCmdOptions = cmdOptions', appLogFunc = logFunc }
    runRIO app inner

displayArgs :: (HasCmdOptions env, HasLogFunc env) => RIO env ()
displayArgs = do
  cmdOptions' <- view cmdOptionsL
  logInfo $ "parsed cmdargs: " <> displayShow cmdOptions'
