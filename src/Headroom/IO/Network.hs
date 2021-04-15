{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
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

import           Data.String.Interpolate             ( i )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import qualified Network.HTTP.Client                as HC
import           Network.HTTP.Req                    ( BsResponse
                                                     , GET(GET)
                                                     , HttpException(..)
                                                     , MonadHttp
                                                     , NoReqBody(NoReqBody)
                                                     , bsResponse
                                                     , defaultHttpConfig
                                                     , req
                                                     , responseBody
                                                     , runReq
                                                     , useURI
                                                     )
import qualified Network.HTTP.Types.Status          as HC
import           RIO
import qualified RIO.Text                           as T
import           Text.URI                            ( URI )
import qualified Text.URI                           as URI


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
  response <- httpGet uri
  case T.decodeUtf8' $ responseBody response of
    Left  err  -> throwM $ InvalidResponse uri (T.pack $ displayException err)
    Right body -> pure body


------------------------------  PRIVATE FUNCTIONS  -----------------------------

httpGet :: (MonadHttp m, MonadThrow m, MonadUnliftIO m) => URI -> m BsResponse
httpGet uri = do
  urlE      <- maybe (throwM $ InvalidURL uri) pure (useURI uri)
  eitherRes <- case urlE of
    Left  url -> doGet $ fst url
    Right url -> doGet $ fst url
  case eitherRes of
    Left  err -> handleHttpException uri err
    Right res -> pure res
 where
  doGet = \u -> try @_ @HttpException $ req GET u NoReqBody bsResponse mempty


handleHttpException :: MonadThrow m => URI -> HttpException -> m BsResponse
handleHttpException uri ex = case ex of
  VanillaHttpException (HC.HttpExceptionRequest _ c) -> case c of
    HC.ConnectionFailure ex' ->
      throwM $ ConnectionFailure uri (T.pack $ displayException ex')
    HC.StatusCodeException response _ ->
      let code    = HC.statusCode . HC.responseStatus $ response
          message = HC.statusMessage . HC.responseStatus $ response
      in  throwM $ InvalidStatus uri code (decodeUtf8Lenient message)
    _ -> throwM ex
  _ -> throwM ex

---------------------------------  ERROR TYPES  --------------------------------

-- | Error related to network operations.
data NetworkError
  = ConnectionFailure URI Text -- ^ connection failure
  | InvalidResponse URI Text        -- ^ error during obtaining response
  | InvalidStatus URI Int Text      -- ^ invalid response status
  | InvalidURL URI                  -- ^ given /URI/ is not valid
  deriving (Eq, Show)


instance Exception NetworkError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: NetworkError -> String
displayException' = \case
  ConnectionFailure uri ex -> [i|Error connecting to #{URI.render uri}: #{ex}|]
  InvalidResponse uri reason ->
    [i|Cannot decode response for '#{URI.render uri}': #{reason}|]
  InvalidStatus uri status message ->
    [i|Error downloading #{URI.render uri}: #{status} #{message}|]
  InvalidURL uri -> [i|Cannot build URL from input URI: #{URI.render uri}|]
