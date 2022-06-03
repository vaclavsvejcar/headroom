{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Template.TemplateRef
-- Description : Representation of reference to template file
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- 'TemplateRef' data type represents reference to template file, either local or
-- remote, which can be later opened/downloaded and parsed into template.
module Headroom.Template.TemplateRef (
    -- * Data Types
      TemplateRef (..)

      -- * Constructor Functions
    , mkTemplateRef

      -- * Public Functions
    , renderRef

      -- * Error Types
    , TemplateRefError (..)
) where

import Data.Aeson (
    FromJSON (..)
    , Value (String)
 )
import Data.String.Interpolate (
    i
    , iii
 )
import Headroom.Data.EnumExtra (textToEnum)
import Headroom.Data.Regex (
    match
    , re
 )
import Headroom.FileType.Types (FileType (..))
import Headroom.Types (
    LicenseType
    , fromHeadroomError
    , toHeadroomError
 )
import RIO
import qualified RIO.Text as T
import Text.URI (
    URI (..)
    , mkURI
 )
import qualified Text.URI as URI

---------------------------------  DATA TYPES  ---------------------------------

-- | Reference to the template (e.g. local file, URI address).
data TemplateRef
    = InlineRef Text
    | -- | template path on local file system
      LocalTemplateRef FilePath
    | -- | remote template URI adress
      UriTemplateRef URI
    | BuiltInRef LicenseType FileType
    deriving (Eq, Ord, Show)

instance FromJSON TemplateRef where
    parseJSON = \case
        String s -> maybe (error $ T.unpack s) pure (mkTemplateRef s)
        other -> error $ "Invalid value for template reference: " <> show other

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Creates a 'TemplateRef' from given text. If the raw text appears to be
-- valid URL with either @http@ or @https@ as protocol, it considers it as
-- 'UriTemplateRef', otherwise it creates 'LocalTemplateRef'.
--
-- >>> mkTemplateRef "/path/to/haskell.mustache" :: Maybe TemplateRef
-- Just (LocalTemplateRef "/path/to/haskell.mustache")
--
-- >>> mkTemplateRef "https://foo.bar/haskell.mustache" :: Maybe TemplateRef
-- Just (UriTemplateRef (URI {uriScheme = Just "https", uriAuthority = Right (Authority {authUserInfo = Nothing, authHost = "foo.bar", authPort = Nothing}), uriPath = Just (False,"haskell.mustache" :| []), uriQuery = [], uriFragment = Nothing}))
mkTemplateRef ::
    MonadThrow m =>
    -- | input text
    Text ->
    -- | created 'TemplateRef' (or error)
    m TemplateRef
mkTemplateRef raw = case match [re|(^\w+):\/\/|] raw of
    Just (_ : p : _)
        | p `elem` ["http", "https"] -> uriTemplateRef
        | otherwise -> throwM $ UnsupportedUriProtocol p raw
    _ -> pure . LocalTemplateRef . T.unpack $ raw
  where
    uriTemplateRef = extractFileType >> UriTemplateRef <$> mkURI raw
    extractFileType = case match [re|(\w+)\.(\w+)$|] raw of
        Just (_ : (textToEnum @FileType -> (Just ft)) : _ : _) -> pure ft
        _ -> throwM $ UnrecognizedTemplateName raw

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Renders given 'TemplateRef' into human-friendly text.
renderRef ::
    -- | 'TemplateRef' to render
    TemplateRef ->
    -- | rendered text
    Text
renderRef (InlineRef content) = [i|<inline template '#{content}'>|]
renderRef (LocalTemplateRef path) = T.pack path
renderRef (UriTemplateRef uri) = URI.render uri
renderRef (BuiltInRef lt ft) = [i|<built-in template #{lt}/#{ft}>|]

---------------------------------  ERROR TYPES  --------------------------------

-- | Error related to template references.
data TemplateRefError
    = -- | not a valid format for template name
      UnrecognizedTemplateName Text
    | -- | URI protocol not supported
      UnsupportedUriProtocol Text Text
    deriving (Eq, Show)

instance Exception TemplateRefError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: TemplateRefError -> String
displayException' = \case
    UnrecognizedTemplateName raw ->
        [iii|
      Cannot extract file type and template type from path #{raw}. Please make
      sure that the path ends with '<FILE_TYPE>.<TEMPLATE_TYPE>', for example
      '/path/to/haskell.mustache'.
    |]
    UnsupportedUriProtocol protocol raw ->
        [iii|
      Protocol '#{protocol}' of in URI '#{raw}' is not supported. Make sure that
      you use either HTTP or HTTPS URIs.
    |]
