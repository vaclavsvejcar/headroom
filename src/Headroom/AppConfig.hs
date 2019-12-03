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
import           Headroom.Types                 ( HeadroomError(..) )
import           Headroom.Types.Util            ( customOptions )
import           RIO
import qualified RIO.ByteString                as B
import           RIO.FilePath                   ( (</>)
                                                , takeDirectory
                                                )
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T


data AppConfig =
  AppConfig { acConfigVersion  :: Int
            , acReplaceHeaders :: Bool
            , acSourcePaths    :: [FilePath]
            , acTemplatePaths  :: [FilePath]
            , acPlaceholders   :: HM.HashMap T.Text T.Text
            } deriving (Eq, Generic, Show)

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

parsePlaceholders :: MonadThrow m => [T.Text] -> m (HM.HashMap T.Text T.Text)
parsePlaceholders placeholders = fmap HM.fromList (mapM parse placeholders)
 where
  parse input = case T.split (== '=') input of
    [key, value] -> return (key, value)
    _            -> throwM $ InvalidPlaceholder input

instance FromJSON AppConfig where
  parseJSON = genericParseJSON customOptions

instance Default AppConfig where
  def = AppConfig 1 False [] [] HM.empty

instance Semigroup AppConfig where
  x <> y = AppConfig (acConfigVersion x `min` acConfigVersion y)
                     (acReplaceHeaders x)
                     (acSourcePaths x <> acSourcePaths y)
                     (acTemplatePaths x <> acTemplatePaths y)
                     (acPlaceholders x <> acPlaceholders y)

instance Monoid AppConfig where
  mempty = def
