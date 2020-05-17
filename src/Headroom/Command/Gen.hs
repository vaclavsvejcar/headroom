{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

{-|
Module      : Headroom.Command.Gen
Description : Handler for the @gen@ command.
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

The @gen@ command is responsible for generating various files requied by
/Headroom/, such as /YAML/ configuration stubs or /Mustache/ license templates.
Run /Headroom/ using the @headroom gen --help@ to see available options.
-}

module Headroom.Command.Gen
  ( commandGen
  , parseGenMode
  )
where


import           Headroom.Command.Utils         ( bootstrap )
import           Headroom.Embedded              ( configFileStub
                                                , licenseTemplate
                                                )
import           Headroom.Types                 ( ApplicationError(..)
                                                , Command(..)
                                                , CommandGenError(..)
                                                , CommandGenOptions(..)
                                                , GenMode(..)
                                                )
import           Prelude                        ( putStrLn )
import           RIO


data Env = Env
  { envLogFunc    :: !LogFunc
  -- ^ logging function
  , envGenOptions :: !CommandGenOptions
  -- ^ options
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

env' :: CommandGenOptions -> LogFunc -> IO Env
env' opts logFunc = pure $ Env { envLogFunc = logFunc, envGenOptions = opts }

-- | Parses 'GenMode' from combination of options from given 'Command'.
parseGenMode :: MonadThrow m
             => Command
             -- ^ command from which to parse the 'GenMode'
             -> m GenMode
             -- ^ parsed 'GenMode'
parseGenMode = \case
  Gen True  Nothing        -> pure GenConfigFile
  Gen False (Just license) -> pure $ GenLicense license
  _                        -> throwM $ CommandGenError NoGenModeSelected

-- | Handler for /Generator/ command.
commandGen :: CommandGenOptions
           -- ^ /Generator/ command options
           -> IO ()
           -- ^ execution result
commandGen opts = bootstrap (env' opts) False $ case cgoGenMode opts of
  GenConfigFile             -> liftIO printConfigFile
  GenLicense (lType, fType) -> liftIO . putStrLn $ licenseTemplate lType fType

printConfigFile :: IO ()
printConfigFile = putStrLn configFileStub
