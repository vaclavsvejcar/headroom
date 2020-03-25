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
  , parseVariables
  , prettyPrintAppConfig
  , validateAppConfig
  )
where

import           Control.Lens
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Validation
import qualified Data.Yaml                     as Y
import           Data.Yaml.Pretty               ( defConfig
                                                , encodePretty
                                                , setConfCompare
                                                )
import           Headroom.AppConfig.Errors      ( AppConfigError(..)
                                                , ValidationError(..)
                                                )
import           Headroom.Types                 ( HeadroomError(..)
                                                , RunMode(..)
                                                )
import           Headroom.Types.Utils           ( customOptions )
import           RIO
import qualified RIO.ByteString                as B
import           RIO.FilePath                   ( takeDirectory
                                                , (</>)
                                                )
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T


-- | Application configuration, loaded either from configuration file or command
-- line options.
data AppConfig = AppConfig
  { acRunMode       :: RunMode           -- ^ selected mode of /Run/ command
  , acSourcePaths   :: [FilePath]        -- ^ paths to source code files
  , acTemplatePaths :: [FilePath]        -- ^ paths to template files
  , acVariables     :: HashMap Text Text -- ^ variables to replace
  }
  deriving (Eq, Generic, Show)

-- | Support for reading configuration from /YAML/.
instance FromJSON AppConfig where
  parseJSON = genericParseJSON customOptions

-- | Support for writing configuration to /YAML/.
instance ToJSON AppConfig where
  toJSON = genericToJSON customOptions

instance Semigroup AppConfig where
  x <> y = AppConfig (acRunMode x)
                     (acSourcePaths x <> acSourcePaths y)
                     (acTemplatePaths x <> acTemplatePaths y)
                     (acVariables x <> acVariables y)

instance Monoid AppConfig where
  mempty = AppConfig Add [] [] HM.empty

-- | Loads and parses application configuration from given file.
loadAppConfig :: MonadIO m
              => FilePath    -- ^ path to configuration file
              -> m AppConfig -- ^ parsed configuration
loadAppConfig path = do
  appConfig <- liftIO $ B.readFile path >>= parseAppConfig
  pure $ makePathsRelativeTo (takeDirectory path) appConfig

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

-- | Parses variables from raw input in @key=value@ format.
--
-- >>> parseVariables ["key1=value1"]
-- fromList [("key1","value1")]
parseVariables :: MonadThrow m
               => [Text]                -- ^ list of raw variables
               -> m (HashMap Text Text) -- ^ parsed variables
parseVariables variables = fmap HM.fromList (mapM parse variables)
 where
  parse input = case T.split (== '=') input of
    [key, value] -> pure (key, value)
    _            -> throwM $ AppConfigError (InvalidVariable input)

-- | Writes the given 'AppConfig' to /YAML/ text, using the pretty printer.
prettyPrintAppConfig :: AppConfig -- ^ application config to write to /YAML/
                     -> Text      -- ^ pretty printed /YAML/ text
prettyPrintAppConfig = decodeUtf8Lenient . encodePretty prettyConfig
  where prettyConfig = setConfCompare compare defConfig

-- | Validates whether given 'AppConfig' contains valid data.
validateAppConfig :: MonadThrow m
                  => AppConfig   -- ^ application config to validate
                  -> m AppConfig -- ^ validated application config (or errors)
validateAppConfig appConfig = case checked of
  Success ac'    -> pure ac'
  Failure errors -> throwM $ AppConfigError (ValidationFailed errors)
 where
  checked          = appConfig <$ checkSourcePaths <* checkTemplatePaths
  checkSourcePaths = if null (acSourcePaths appConfig)
    then _Failure # [EmptySourcePaths]
    else _Success # appConfig
  checkTemplatePaths = if null (acTemplatePaths appConfig)
    then _Failure # [EmptyTemplatePaths]
    else _Success # appConfig
