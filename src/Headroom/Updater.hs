{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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
  ( parseLatestVersion
    -- * Error Data Types
  , UpdaterError(..)
  )
where

import           Data.Aeson                          ( Value(String) )
import           Data.String.Interpolate             ( iii )
import           Headroom.Meta.Version               ( Version
                                                     , parseVersion
                                                     )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           Lens.Micro.Aeson                    ( key )
import           RIO


-- | Parses latest version number from /GitHub/ API response.
parseLatestVersion :: MonadThrow m
                   => Value     -- ^ raw JSON response from /GitHub/
                   -> m Version -- ^ parsed version
parseLatestVersion json = case json ^? key "name" of
  Just (String rawValue) -> case parseVersion rawValue of
    Just version -> pure version
    _            -> throwM CannotDetectVersion
  _ -> throwM CannotDetectVersion

---------------------------------  ERROR TYPES  --------------------------------

-- | Error during processing updates.
data UpdaterError = CannotDetectVersion -- ^ latest version cannot be detected
  deriving (Eq, Show, Typeable)

instance Exception UpdaterError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: UpdaterError -> String
displayException' = \case
  CannotDetectVersion -> [iii|
        Cannot get latest Headroom version from update servers.
      |]
