{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  { envLogFunc    :: !LogFunc           -- ^ logging function
  , envGenOptions :: !CommandGenOptions -- ^ options
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

env' :: CommandGenOptions -> LogFunc -> IO Env
env' opts logFunc = pure $ Env { envLogFunc = logFunc, envGenOptions = opts }

parseGenMode :: MonadThrow m => Command -> m GenMode
parseGenMode = \case
  Gen True  Nothing        -> pure GenConfigFile
  Gen False (Just license) -> pure $ GenLicense license
  _                        -> throwM $ CommandGenError NoGenModeSelected

-- | Handler for /Generator/ command.
commandGen :: CommandGenOptions -- ^ /Generator/ command options
           -> IO ()             -- ^ execution result
commandGen opts = bootstrap (env' opts) False $ case cgoGenMode opts of
  GenConfigFile             -> liftIO printConfigFile
  GenLicense (lType, fType) -> liftIO . putStrLn $ licenseTemplate lType fType

printConfigFile :: IO ()
printConfigFile = putStrLn configFileStub
