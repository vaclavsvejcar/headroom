{-|
Module      : Headroom.AppConfig
Description : Application configuration
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types and functions for representing and handling application config.
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
import qualified RIO.HashMap                   as HM
import           RIO.Text                       ( Text )
import qualified RIO.Text                      as T


data AppConfig = AppConfig
  { acConfigVersion :: Int
  , acRunMode       :: RunMode
  , acSourcePaths   :: [FilePath]
  , acTemplatePaths :: [FilePath]
  , acPlaceholders  :: HM.HashMap Text Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON AppConfig where
  parseJSON = genericParseJSON customOptions

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

loadAppConfig :: MonadIO m => FilePath -> m AppConfig
loadAppConfig path = do
  appConfig <- liftIO $ B.readFile path >>= parseAppConfig
  return $ makePathsRelativeTo (takeDirectory path) appConfig

makePathsRelativeTo :: FilePath -> AppConfig -> AppConfig
makePathsRelativeTo root appConfig = appConfig
  { acSourcePaths   = processPaths . acSourcePaths $ appConfig
  , acTemplatePaths = processPaths . acTemplatePaths $ appConfig
  }
  where processPaths = fmap (root </>)

parseAppConfig :: MonadThrow m => B.ByteString -> m AppConfig
parseAppConfig = Y.decodeThrow

parsePlaceholders :: MonadThrow m => [Text] -> m (HM.HashMap Text Text)
parsePlaceholders placeholders = fmap HM.fromList (mapM parse placeholders)
 where
  parse input = case T.split (== '=') input of
    [key, value] -> return (key, value)
    _            -> throwM $ InvalidPlaceholder input
