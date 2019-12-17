{-|
Module      : Headroom.Command.Gen
Description : Logic for Generate command
Copyright   : (c) 2019 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Logic for the @gen@ command, used to generate /stub/ files.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Gen
  ( commandGen
  )
where

import           Headroom.Command.Gen.Env
import           Headroom.Command.Shared        ( bootstrap )
import           Headroom.Embedded              ( configFileStub )
import           Prelude                        ( putStrLn )
import           RIO
import qualified RIO.Text                      as T

env' :: GenOptions -> LogFunc -> IO Env
env' opts logFunc = return $ Env { envLogFunc = logFunc, envGenOptions = opts }

commandGen :: GenOptions -> IO ()
commandGen opts = bootstrap (env' opts) (goDebug opts) $ case goGenMode opts of
  GenConfigFile      -> liftIO printConfigFile
  GenLicense license -> liftIO $ printLicense license

printConfigFile :: IO ()
printConfigFile = putStrLn configFileStub

printLicense :: T.Text -> IO ()
printLicense _ = putStrLn "not implemented yet"
