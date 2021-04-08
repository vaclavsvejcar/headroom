{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}

{-|
Module      : Headroom.Template.TemplateRef
Description : Representation of reference to template file
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

'TemplateRef' data type represents reference to template file, either local or
remote, which can be later opened/downloaded and parsed into template.
-}

module Headroom.Template.TemplateRef
  ( -- * Data Types
    TemplateRef(..)
    -- * Constructor Functions
  , mkTemplateRef
    -- * Error Types
  , TemplateRefError(..)
  )
where

import           Data.String.Interpolate             ( iii )
import           Headroom.Data.Regex                 ( match
                                                     , re
                                                     )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           RIO
import qualified RIO.Text                           as T
import           Text.URI                            ( URI(..)
                                                     , mkURI
                                                     )


---------------------------------  DATA TYPES  ---------------------------------

-- | Reference to the template (e.g. local file, URI address).
data TemplateRef
  = LocalTemplateRef FilePath -- ^ template path on local file system
  | UriTemplateRef URI        -- ^ remote template URI adress
  deriving (Eq, Ord, Show)


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
mkTemplateRef :: MonadThrow m
              => Text          -- ^ input text
              -> m TemplateRef -- ^ created 'TemplateRef' (or error)
mkTemplateRef raw = case match [re|(^\w+):\/\/|] raw of
  Just (_ : p : _) | p `elem` ["http", "https"] -> UriTemplateRef <$> mkURI raw
                   | otherwise -> throwM $ UnsupportedUriProtocol p raw
  _ -> pure . LocalTemplateRef . T.unpack $ raw


---------------------------------  ERROR TYPES  --------------------------------

-- | Error related to template references.
data TemplateRefError
  = UnrecognizedTemplateName Text    -- ^ not a valid format for template name
  | UnsupportedUriProtocol Text Text -- ^ URI protocol not supported
  deriving (Eq, Show)


instance Exception TemplateRefError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: TemplateRefError -> String
displayException' = \case
  UnrecognizedTemplateName raw -> [iii|
      Cannot extract file type and template type from path #{raw}. Please make
      sure that the path ends with '<FILE_TYPE>.<TEMPLATE_TYPE>', for example
      '/path/to/haskell.mustache'.
    |]
  UnsupportedUriProtocol protocol raw -> [iii|
      Protocol '#{protocol}' of in URI '#{raw}' is not supported. Make sure that
      you use either HTTP or HTTPS URIs.
    |]
