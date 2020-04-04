{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Utils
  ( bootstrap
  )
where

import           RIO

-- | Bootstraps /RIO/ application using provided environment data and flag
-- whether to run in debug mode.
bootstrap :: (LogFunc -> IO env) -- ^ function returning environment data
          -> Bool                -- ^ whether to run in debug mode
          -> RIO env a           -- ^ /RIO/ application to execute
          -> IO a                -- ^ execution result
bootstrap getEnv isDebug logic = do
  logOptions <- logOptionsHandle stderr isDebug
  let logOptions' = setLogUseLoc False logOptions
  withLogFunc logOptions' $ \logFunc -> do
    env <- liftIO $ getEnv logFunc
    runRIO env logic

