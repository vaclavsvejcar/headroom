{-|
Module      : Headroom.Command.Run.Env
Description : Environment for the Run command
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and instances for the /Run/ command environment.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Command.Run.Env
  ( RunOptions(..)
  , StartupEnv(..)
  , Env(..)
  , HasAppConfig(..)
  , HasRunOptions(..)
  , toAppConfig
  )
where

import           Headroom.AppConfig             ( AppConfig(..)
                                                , parseVariables
                                                )
import           Headroom.Types                 ( RunMode )
import           RIO


-- | Options for the /Run/ command.
data RunOptions = RunOptions
  { roRunMode       :: RunMode    -- ^ used /Run/ command mode
  , roSourcePaths   :: [FilePath] -- ^ source code file paths
  , roTemplatePaths :: [FilePath] -- ^ template file paths
  , roVariables     :: [Text]     -- ^ raw variables
  , roDebug         :: Bool       -- ^ whether to run in debug mode
  }
  deriving (Eq, Show)

-- | Initial /RIO/ startup environment for the /Run/ command.
data StartupEnv = StartupEnv
  { envLogFunc    :: !LogFunc    -- ^ logging function
  , envRunOptions :: !RunOptions -- ^ options
  }

-- | Full /RIO/ environment for the /Run/ command.
data Env = Env
  { envEnv       :: !StartupEnv -- ^ startup /RIO/ environment
  , envAppConfig :: !AppConfig  -- ^ application configuration
  }

-- | Environment value with application configuration.
class HasAppConfig env where
  -- | Application config lens.
  appConfigL :: Lens' env AppConfig

class (HasLogFunc env, HasRunOptions env) => HasEnv env where
  envL :: Lens' env StartupEnv

-- | Environment value with /Run/ command options.
class HasRunOptions env where
  -- | /Run/ command options lens.
  runOptionsL :: Lens' env RunOptions

instance HasAppConfig Env where
  appConfigL = lens envAppConfig (\x y -> x { envAppConfig = y })

instance HasEnv StartupEnv where
  envL = id

instance HasEnv Env where
  envL = lens envEnv (\x y -> x { envEnv = y })

instance HasLogFunc StartupEnv where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasLogFunc Env where
  logFuncL = envL . logFuncL

instance HasRunOptions StartupEnv where
  runOptionsL = lens envRunOptions (\x y -> x { envRunOptions = y })

instance HasRunOptions Env where
  runOptionsL = envL . runOptionsL

-- | Converts options for /Run/ command into application config.
toAppConfig :: MonadThrow m
            => RunOptions  -- ^ /Run/ command options
            -> m AppConfig -- ^ application configuration
toAppConfig opts = do
  variables' <- parseVariables (roVariables opts)
  return $ mempty { acSourcePaths   = roSourcePaths opts
                  , acTemplatePaths = roTemplatePaths opts
                  , acRunMode       = roRunMode opts
                  , acVariables     = variables'
                  }
