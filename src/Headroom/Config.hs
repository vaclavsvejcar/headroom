module Headroom.Config
  ( loadAppConfig
  , parseAppConfig
  )
where

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as B
import           Headroom.Types                 ( AppConfig )

loadAppConfig :: FilePath -> IO B.ByteString
loadAppConfig = B.readFile

parseAppConfig :: B.ByteString -> Maybe AppConfig
parseAppConfig = A.decode
