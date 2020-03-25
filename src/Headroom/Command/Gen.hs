{-|
Module      : Headroom.Command.Gen
Description : Logic for Generate command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Logic for the /Generator/ command, used to generate /stub/ files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Gen
  ( commandGen
  )
where

import           Headroom.Command.Gen.Env
import           Headroom.Command.Gen.Errors    ( GenCommandError(..) )
import           Headroom.Command.Shared        ( bootstrap )
import           Headroom.Embedded              ( configFileStub
                                                , licenseTemplate
                                                )
import           Headroom.License               ( parseLicense )
import           Headroom.Types                 ( HeadroomError(..) )
import           Prelude                        ( putStrLn )
import           RIO


env' :: GenOptions -> LogFunc -> IO Env
env' opts logFunc = pure $ Env { envLogFunc = logFunc, envGenOptions = opts }

-- | Handler for /Generator/ command.
commandGen :: GenOptions -- ^ /Generator/ command options
           -> IO ()      -- ^ execution result
commandGen opts = bootstrap (env' opts) False $ case goGenMode opts of
  GenConfigFile      -> liftIO printConfigFile
  GenLicense license -> liftIO $ printLicense license

printConfigFile :: IO ()
printConfigFile = putStrLn configFileStub

printLicense :: Text -> IO ()
printLicense license = case parseLicense license of
  Just license' -> putStrLn $ licenseTemplate license'
  Nothing       -> throwM $ GenCommandError (InvalidLicense license)
