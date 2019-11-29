{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Run.Env
  ( RunOptions(..)
  , StartupEnv(..)
  , Env(..)
  , HasAppConfig(..)
  , HasRunOptions(..)
  , toAppConfig
  )
where

import           Data.Default                   ( def )
import           Headroom.Types                 ( AppConfig(..) )
import           RIO

data RunOptions =
    RunOptions { roReplaceHeaders :: Bool
               , roSourcePaths    :: [FilePath]
               , roTemplatePaths  :: [FilePath]
               , roDebug          :: Bool
               } deriving (Eq, Show)

data StartupEnv =
    StartupEnv { envLogFunc    :: !LogFunc
               , envRunOptions :: !RunOptions
               }

data Env =
    Env { envEnv :: !StartupEnv
        , envAppConfig :: !AppConfig}

class HasAppConfig env where
  appConfigL :: Lens' env AppConfig

class (HasLogFunc env, HasRunOptions env) => HasEnv env where
  envL :: Lens' env StartupEnv

class HasRunOptions env where
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

toAppConfig :: RunOptions -> AppConfig
toAppConfig opts = def { acSourcePaths    = roSourcePaths opts
                       , acTemplatePaths  = roTemplatePaths opts
                       , acReplaceHeaders = roReplaceHeaders opts
                       }
