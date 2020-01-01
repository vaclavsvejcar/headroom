{-|
Module      : Headroom.Command.Shared
Description : Shared code for commands
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Shared functionality for the individual command implementations.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Shared
  ( bootstrap
  )
where

import           RIO


bootstrap :: (LogFunc -> IO env) -> Bool -> RIO env a -> IO a
bootstrap getEnv isDebug logic = do
  logOptions <- logOptionsHandle stderr isDebug
  let logOptions' = setLogUseLoc False logOptions
  withLogFunc logOptions' $ \logFunc -> do
    env <- liftIO $ getEnv logFunc
    runRIO env logic
