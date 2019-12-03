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
