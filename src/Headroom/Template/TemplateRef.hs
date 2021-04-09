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
{-# LANGUAGE ViewPatterns          #-}

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
    -- * Public Functions
  , renderRef
    -- * Error Types
  , TemplateRefError(..)
  )
where

import           Data.String.Interpolate             ( iii )
import           Headroom.Data.EnumExtra             ( textToEnum )
import           Headroom.Data.Regex                 ( match
                                                     , re
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           RIO
import qualified RIO.Text                           as T
import           Text.URI                            ( URI(..)
                                                     , mkURI
                                                     )
import qualified Text.URI                           as URI


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
-- >>> :set -XTypeApplications
-- >>> import Headroom.Template.Mustache (Mustache)
-- >>> mkTemplateRef @Mustache "/path/to/haskell.mustache" :: Maybe TemplateRef
-- Just (LocalTemplateRef "/path/to/haskell.mustache")
--
-- >>> :set -XTypeApplications
-- >>> import Headroom.Template.Mustache (Mustache)
-- >>> mkTemplateRef @Mustache "https://foo.bar/haskell.mustache" :: Maybe TemplateRef
-- Just (UriTemplateRef (URI {uriScheme = Just "https", uriAuthority = Right (Authority {authUserInfo = Nothing, authHost = "foo.bar", authPort = Nothing}), uriPath = Just (False,"haskell.mustache" :| []), uriQuery = [], uriFragment = Nothing}))
mkTemplateRef :: forall a m
               . (Template a, MonadThrow m)
              => Text          -- ^ input text
              -> m TemplateRef -- ^ created 'TemplateRef' (or error)
mkTemplateRef raw = case match [re|(^\w+):\/\/|] raw of
  Just (_ : p : _) | p `elem` ["http", "https"] -> uriTemplateRef
                   | otherwise -> throwM $ UnsupportedUriProtocol p raw
  _ -> pure . LocalTemplateRef . T.unpack $ raw
 where
  uriTemplateRef  = extractFileType >> UriTemplateRef <$> mkURI raw
  exts            = templateExtensions @a
  extractFileType = case match [re|(\w+)\.(\w+)$|] raw of
    Just (_ : (textToEnum @FileType -> (Just ft )) : e : _) | e `elem` exts ->
      pure ft
    _ -> throwM $ UnrecognizedTemplateName raw


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Renders given 'TemplateRef' into human-friendly text.
renderRef :: TemplateRef -- ^ 'TemplateRef' to render
          -> Text        -- ^ rendered text
renderRef (LocalTemplateRef path) = T.pack path
renderRef (UriTemplateRef   uri ) = URI.render uri


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
