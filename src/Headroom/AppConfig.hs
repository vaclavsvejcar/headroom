{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.AppConfig
  ( loadAppConfig
  , makePathsRelativeTo
  , parseAppConfig
  , parsePlaceholders
  )
where

import qualified Data.Yaml                     as Y
import           Headroom.Types                 ( AppConfig(..)
                                                , HeadroomError(..)
                                                )
import           RIO
import qualified RIO.ByteString                as B
import           RIO.FilePath                   ( (</>)
                                                , takeDirectory
                                                )
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T


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
