{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Headroom.Updater
Description : Update Manager for Headroom
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

/Update Manager (Updater)/ is responsible for fetching data about latest version
of /Headroom/ and informing user about available updates. In future versions, it
might be capable to update /Headroom/ binaries automatically.
-}

module Headroom.Updater
  ( fetchLatestVersion
  , parseLatestVersion
    -- * Error Data Types
  , UpdaterError(..)
  )
where

import           Data.Aeson                          ( Value(String) )
import qualified Data.Aeson                         as A
import           Data.String.Interpolate             ( iii )
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.IO.Network                 ( Network(..) )
import           Headroom.Meta.Version               ( Version
                                                     , parseVersion
                                                     )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           Lens.Micro.Aeson                    ( key )
import           RIO
import qualified RIO.ByteString.Lazy                as BL
import qualified RIO.Text                           as T
import           Text.URI                            ( URI )
import qualified Text.URI                           as URI


-- | Fetches and parses latest version from update server.
fetchLatestVersion :: (Has (Network (RIO env)) env) => RIO env Version
fetchLatestVersion = do
  Network {..} <- viewL
  apiURI       <- latestVersionApiURI
  resp         <- catchAny (nDownloadContent apiURI) handleError
  case A.decode (BL.fromStrict resp) of
    Just json -> parseLatestVersion json
    _         -> throwM $ CannotDetectVersion "cannot parse response"
  where handleError = throwM . CannotDetectVersion . T.pack . displayException


-- | Parses latest version number from /GitHub/ API response.
parseLatestVersion :: MonadThrow m
                   => Value     -- ^ raw JSON response from /GitHub/
                   -> m Version -- ^ parsed version
parseLatestVersion json = case json ^? key "name" of
  Just (String rawValue) -> case parseVersion rawValue of
    Just version -> pure version
    _            -> throwM $ CannotDetectVersion "cannot parse version"
  _ -> throwM $ CannotDetectVersion "cannot parse response"


latestVersionApiURI :: MonadThrow m => m URI
latestVersionApiURI = URI.mkURI
  "https://api.github.com/repos/vaclavsvejcar/headroom/releases/latest"


---------------------------------  ERROR TYPES  --------------------------------

-- | Error during processing updates.
data UpdaterError = CannotDetectVersion Text
  deriving (Eq, Show, Typeable)

instance Exception UpdaterError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: UpdaterError -> String
displayException' = \case
  CannotDetectVersion reason -> [iii|
    Cannot get latest Headroom version from update servers, reason: #{reason}.
  |]
