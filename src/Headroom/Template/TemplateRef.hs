{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

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
    TemplateSource(..)
  , TemplateRef(..)
    -- * Constructor Functions
  , mkTemplateRef
    -- * Error Types
  , TemplateRefError(..)
  )
where

import           Data.String.Interpolate             ( iii )
import           Headroom.Data.EnumExtra             ( textToEnum )
import           Headroom.Data.Regex                 ( match
                                                     , re
                                                     )
import           Headroom.FileType.Types             ( FileType )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Types                      ( fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           RIO
import qualified RIO.Text                           as T
import           Text.URI                            ( URI(..)
                                                     , mkURI
                                                     )


---------------------------------  DATA TYPES  ---------------------------------

-- | Source of the template (e.g. local file, URI address).
data TemplateSource
  = LocalTemplateSource FilePath -- ^ template path on local file system
  | UriTemplateSource URI        -- ^ remote template URI adress
  deriving (Eq, Show)


-- | Reference to the template. Later this reference is used to get and parse
-- the content of the actual template.
data TemplateRef = TemplateRef
  { trFileType :: FileType       -- ^ type of files which this template is for
  , trSource   :: TemplateSource -- ^ source of the template
  }
  deriving (Eq, Show)


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Creates a 'TemplateRef' from given text. If the raw text appears to be
-- valid URL with either @http@ or @https@ as protocol, it considers it as
-- 'UriTemplateSource', otherwise it creates 'LocalTemplateSource'.
--
-- >>> :set -XTypeApplications
-- >>> import Headroom.Template.Mustache (Mustache)
-- >>> mkTemplateRef @Mustache "/path/to/haskell.mustache" :: Maybe TemplateRef
-- Just (TemplateRef {trFileType = Haskell, trSource = LocalTemplateSource "/path/to/haskell.mustache"})
--
-- >>> :set -XTypeApplications
-- >>> import Headroom.Template.Mustache (Mustache)
-- >>> mkTemplateRef @Mustache "https://foo.bar/haskell.mustache" :: Maybe TemplateRef
-- Just (TemplateRef {trFileType = Haskell, trSource = UriTemplateSource (URI {uriScheme = Just "https", uriAuthority = Right (Authority {authUserInfo = Nothing, authHost = "foo.bar", authPort = Nothing}), uriPath = Just (False,"haskell.mustache" :| []), uriQuery = [], uriFragment = Nothing})})
mkTemplateRef :: forall a m
               . (Template a, MonadThrow m)
              => Text          -- ^ input text
              -> m TemplateRef -- ^ created 'TemplateRef' (or error)
mkTemplateRef raw = do
  fileType <- extractFileType
  source   <- detectSource
  pure TemplateRef { trFileType = fileType, trSource = source }
 where
  exts         = templateExtensions @a
  detectSource = case match [re|(^\w+):\/\/|] raw of
    Just (_ : p : _)
      | p `elem` ["http", "https"] -> UriTemplateSource <$> mkURI raw
      | otherwise                  -> throwM $ UnsupportedUriProtocol p raw
    _ -> pure . LocalTemplateSource . T.unpack $ raw
  extractFileType = case match [re|(\w+)\.(\w+)$|] raw of
    Just (_ : (textToEnum -> (Just ft )) : e : _) | e `elem` exts -> pure ft
    _ -> throwM $ UnrecognizedTemplateName raw


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
