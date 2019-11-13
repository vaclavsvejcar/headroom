module Headroom.Config
  ( loadAppConfig
  , parseAppConfig
  )
where

import           Control.Arrow                  ( left )
import qualified Data.ByteString               as B
import qualified Data.Yaml                     as Y
import           Headroom.Types                 ( AppConfig )

loadAppConfig :: FilePath -> IO B.ByteString
loadAppConfig = B.readFile

parseAppConfig :: B.ByteString -> Either String AppConfig
parseAppConfig raw = left
  (\err -> "Cannot parse configuration: " ++ show err)
  (Y.decodeEither' raw)
