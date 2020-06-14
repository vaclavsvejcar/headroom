{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.Command.Types
Description : Data types for "Headroom.Command"
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types for "Headroom.Command" module.
-}

module Headroom.Command.Types
  ( Command(..)
  , CommandGenOptions(..)
  , CommandInitOptions(..)
  , CommandRunOptions(..)
  )
where

import           Headroom.Configuration.Types   ( GenMode
                                                , LicenseType
                                                , RunMode
                                                , TemplateSource
                                                )
import           Headroom.Data.Regex            ( Regex )
import           Headroom.FileType.Types        ( FileType )
import           RIO


-- | Application command.
data Command
  = Run [FilePath] [Regex] (Maybe TemplateSource) [Text] (Maybe RunMode) Bool Bool
  -- ^ @run@ command
  | Gen Bool (Maybe (LicenseType, FileType))
  -- ^ @gen@ command
  | Init LicenseType [FilePath]
  -- ^ @init@ command
  deriving (Show)


-- | Options for the @gen@ command.
newtype CommandGenOptions = CommandGenOptions
  { cgoGenMode :: GenMode
  -- ^ selected mode
  }
  deriving (Show)

-- | Options for the @init@ command.
data CommandInitOptions = CommandInitOptions
  { cioSourcePaths :: ![FilePath]
  -- ^ paths to source code files
  , cioLicenseType :: !LicenseType
  -- ^ license type
  }
  deriving Show

-- | Options for the @run@ command.
data CommandRunOptions = CommandRunOptions
  { croRunMode        :: !(Maybe RunMode)
  -- ^ used /Run/ command mode
  , croSourcePaths    :: ![FilePath]
  -- ^ source code file paths
  , croExcludedPaths  :: ![Regex]
  -- ^ source paths to exclude
  , croTemplateSource :: !(Maybe TemplateSource)
  -- ^ source of license templates
  , croVariables      :: ![Text]
  -- ^ raw variables
  , croDebug          :: !Bool
  -- ^ whether to run in debug mode
  , croDryRun         :: !Bool
  -- ^ whether to perform dry run
  }
  deriving (Eq, Show)
