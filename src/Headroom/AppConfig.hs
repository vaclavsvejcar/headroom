{-|
Module      : Headroom.AppConfig
Description : Application configuration
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module adds support for loading and parsing application configuration.
Such configuration can be loaded either from /YAML/ config file, or from command
line arguments. Provided 'Semigroup' and 'Monoid' instances allows to merge
multiple loaded configurations into one.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.AppConfig
  ( AppConfig(..)
  , loadAppConfig
  , makePathsRelativeTo
  , parseAppConfig
  , parsePlaceholders
  )
where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , genericParseJSON
                                                )
import           Data.Default                   ( Default
                                                , def
                                                )
import qualified Data.Yaml                     as Y
import           Headroom.Types                 ( HeadroomError(..)
                                                , RunMode(..)
                                                )
import           Headroom.Types.Utils           ( customOptions )
import           RIO
import qualified RIO.ByteString                as B
import           RIO.FilePath                   ( takeDirectory
                                                , (</>)
                                                )
import           RIO.HashMap                    ( HashMap )
import qualified RIO.HashMap                   as HM
import           RIO.Text                       ( Text )
import qualified RIO.Text                      as T


-- | Application configuration, loaded either from configuration file or command
-- line options.
data AppConfig = AppConfig
  { acConfigVersion :: Int               -- ^ version of configuration
  , acRunMode       :: RunMode           -- ^ selected mode of /Run/ command
  , acSourcePaths   :: [FilePath]        -- ^ paths to source code files
  , acTemplatePaths :: [FilePath]        -- ^ paths to template files
  , acPlaceholders  :: HashMap Text Text -- ^ placeholders to replace
  }
  deriving (Eq, Generic, Show)

-- | Support for reading configuration from /YAML/.
instance FromJSON AppConfig where
  parseJSON = genericParseJSON customOptions

-- | Default empty configuration.
instance Default AppConfig where
  def = AppConfig 1 Add [] [] HM.empty

instance Semigroup AppConfig where
  x <> y = AppConfig (acConfigVersion x `min` acConfigVersion y)
                     (acRunMode x)
                     (acSourcePaths x <> acSourcePaths y)
                     (acTemplatePaths x <> acTemplatePaths y)
                     (acPlaceholders x <> acPlaceholders y)

instance Monoid AppConfig where
  mempty = def

-- | Loads and parses application configuration from given file.
loadAppConfig :: MonadIO m
              => FilePath    -- ^ path to configuration file
              -> m AppConfig -- ^ parsed configuration
loadAppConfig path = do
  appConfig <- liftIO $ B.readFile path >>= parseAppConfig
  return $ makePathsRelativeTo (takeDirectory path) appConfig

-- | Rewrites all file paths in 'AppConfig' to be relative to given file path.
makePathsRelativeTo :: FilePath  -- ^ file path to use
                    -> AppConfig -- ^ input application configuration
                    -> AppConfig -- ^ result with relativized file paths
makePathsRelativeTo root appConfig = appConfig
  { acSourcePaths   = processPaths . acSourcePaths $ appConfig
  , acTemplatePaths = processPaths . acTemplatePaths $ appConfig
  }
  where processPaths = fmap (root </>)

-- | Parses application configuration from given raw input.
parseAppConfig :: MonadThrow m
               => B.ByteString -- ^ raw input to parse
               -> m AppConfig  -- ^ parsed application configuration
parseAppConfig = Y.decodeThrow

-- | Parses placeholders from raw input in @key=value@ format.
--
-- >>> parsePlaceholders ["key1=value1"]
-- fromList [("key1","value1")]
parsePlaceholders :: MonadThrow m
                  => [Text]                -- ^ list of raw placeholders
                  -> m (HashMap Text Text) -- ^ parsed placeholders
parsePlaceholders placeholders = fmap HM.fromList (mapM parse placeholders)
 where
  parse input = case T.split (== '=') input of
    [key, value] -> return (key, value)
    _            -> throwM $ InvalidPlaceholder input
