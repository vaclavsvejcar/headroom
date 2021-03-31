{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Headroom.IO.Network
Description : Network related IO operations
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing support to perform selected network IO operations, such as
downloading file content, etc.
-}

module Headroom.IO.Network
  ( -- * Type Aliases
    DownloadContentFn
    -- * Polymorphic Record
  , Network(..)
  , mkNetwork
    -- * Network IO operations
  , downloadContent
  )
where

import           Data.String.Interpolate             ( iii )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           Network.HTTP.Req                    ( GET(GET)
                                                     , NoReqBody(NoReqBody)
                                                     , bsResponse
                                                     , defaultHttpConfig
                                                     , req
                                                     , responseBody
                                                     , runReq
                                                     , useURI
                                                     )
import           RIO
import qualified RIO.Text                           as T
import           Text.URI                            ( URI )


--------------------------------  TYPE ALIASES  --------------------------------

-- | Type of a function that returns content of remote resource.
type DownloadContentFn m
  =  URI    -- ^ /URI/ of remote resource
  -> m Text -- ^ downloaded content


-----------------------------  POLYMORPHIC RECORD  -----------------------------

-- | Polymorphic record of functions performing network IO operations.
data Network m = Network
  { nDownloadContent :: DownloadContentFn m -- ^ downloads remote content
  }


-- | Constructs new 'Network' that performs real network /IO/ operations.
mkNetwork :: MonadIO m => Network m
mkNetwork = Network { nDownloadContent = downloadContent }


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Downloads content of remote resource as 'Text'. Note that only @http@ and
-- @https@ is supported at this moment.
downloadContent :: MonadIO m
                => URI    -- ^ /URI/ of remote resource
                -> m Text -- ^ downloaded content
downloadContent uri = runReq defaultHttpConfig $ do
  urlE     <- maybe (throwM $ InvalidURL uri) pure (useURI uri)
  response <- case urlE of
    Left  httpUrl  -> req GET (fst httpUrl) NoReqBody bsResponse mempty
    Right httpsUrl -> req GET (fst httpsUrl) NoReqBody bsResponse mempty
  case T.decodeUtf8' $ responseBody response of
    Left  err  -> throwM $ InvalidResponse uri (T.pack $ displayException err)
    Right body -> pure body


---------------------------------  ERROR TYPES  --------------------------------

-- | Error related to network operations.
data NetworkError
  = InvalidResponse URI Text -- ^ error during obtaining response
  | InvalidURL URI           -- ^ given /URI/ is not valid
  deriving (Eq, Show)


instance Exception NetworkError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: NetworkError -> String
displayException' = \case
  InvalidResponse uri reason ->
    [iii|Cannot decode response for '#{uri}': #{reason}|]
  InvalidURL uri -> [iii|Cannot build URL from input URI: #{uri}|]
