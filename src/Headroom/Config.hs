{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Config
  ( loadAppConfig
  , parseAppConfig
  )
where

import           Control.Arrow                  ( left )
import qualified Data.Yaml                     as Y
import           Headroom.Types                 ( AppConfig )
import           RIO
import qualified RIO.ByteString                as B

loadAppConfig :: FilePath -> IO B.ByteString
loadAppConfig = B.readFile

parseAppConfig :: B.ByteString -> Either String AppConfig
parseAppConfig raw = left
  (\err -> "Cannot parse configuration: " ++ show err)
  (Y.decodeEither' raw)
