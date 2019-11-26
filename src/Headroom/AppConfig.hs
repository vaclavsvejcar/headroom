{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.AppConfig
  ( loadAppConfig
  , parseAppConfig
  )
where

import qualified Data.Yaml                     as Y
import           Headroom.Types                 ( AppConfig )
import           RIO
import qualified RIO.ByteString                as B


loadAppConfig :: MonadIO m => FilePath -> m AppConfig
loadAppConfig path = liftIO $ B.readFile path >>= parseAppConfig

parseAppConfig :: MonadThrow m => B.ByteString -> m AppConfig
parseAppConfig raw = case Y.decodeEither' raw of
  Left  err    -> throwM err
  Right config -> return config
