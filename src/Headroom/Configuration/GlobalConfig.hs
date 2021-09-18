{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Configuration.GlobalConfig
Description : Global configutation
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

/Global configuration/ is configuration shared between all /Headroom/ instances
and it's located in user's home directory.
-}

module Headroom.Configuration.GlobalConfig
  ( GlobalConfig(..)
  , initGlobalConfigIfNeeded
  , loadGlobalConfig
  )
where

import           Data.Aeson                          ( FromJSON(..)
                                                     , genericParseJSON
                                                     )
import qualified Data.Yaml                          as Y
import           Headroom.Data.Has                   ( Has(..)
                                                     , HasRIO
                                                     )
import           Headroom.Data.Serialization         ( aesonOptions )
import           Headroom.Embedded                   ( defaultGlobalConfig )
import           Headroom.IO.FileSystem              ( FileSystem(..) )
import           Headroom.Meta                       ( globalConfigDirName
                                                     , globalConfigFileName
                                                     )
import           RIO
import qualified RIO.ByteString                     as B
import           RIO.FilePath                        ( (</>) )


---------------------------------  DATA TYPES  ---------------------------------

-- | Data type representing global configuration options.
data GlobalConfig = GlobalConfig
  { gcCheckForUpdates :: Bool -- ^ whether to check for updates
  }
  deriving (Eq, Generic, Show)

instance FromJSON GlobalConfig where
  parseJSON = genericParseJSON aesonOptions


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Checks if global configuration /YAML/ file is already present and if not,
-- it creates one with default values.
initGlobalConfigIfNeeded :: (HasRIO FileSystem env) => RIO env ()
initGlobalConfigIfNeeded = do
  FileSystem {..} <- viewL
  userDir         <- fsGetUserDirectory
  whenM (not <$> fsDoesFileExist (userDir </> configPath)) $ do
    fsCreateDirectory $ userDir </> globalConfigDirName
    fsWriteFile (userDir </> configPath) defaultGlobalConfig


-- | Loads global configuration from /YAML/ file.
loadGlobalConfig :: (HasRIO FileSystem env) => RIO env GlobalConfig
loadGlobalConfig = do
  FileSystem {..} <- viewL
  userDir         <- fsGetUserDirectory
  content         <- liftIO . B.readFile $ (userDir </> configPath)
  Y.decodeThrow content


------------------------------  PRIVATE FUNCTIONS  -----------------------------

configPath :: FilePath
configPath = globalConfigDirName </> globalConfigFileName
