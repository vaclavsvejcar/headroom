{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Utils
-- Description : Shared code for individual command handlers
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains shared code common to all command handlers.
module Headroom.Command.Utils (
    bootstrap
) where

import RIO

-- | Bootstraps /RIO/ application using provided environment data and flag
-- whether to run in debug mode.
bootstrap ::
    -- | function returning environment data
    (LogFunc -> IO env) ->
    -- | whether to run in debug mode
    Bool ->
    -- | /RIO/ application to execute
    RIO env a ->
    -- | execution result
    IO a
bootstrap enfFn isDebug logic = do
    defLogOptions <- logOptionsHandle stderr isDebug
    withLogFunc (setLogUseLoc False defLogOptions) $ \logFunc -> do
        env <- liftIO $ enfFn logFunc
        runRIO env logic
