{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}

{-|
Module      : Headroom.Template
Description : Generic representation of supported template type
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing generic representation of supported template type, using
the 'Template' /type class/.
-}

module Headroom.Template where

import           RIO

-- | Type class representing generic license header template support.
class Template t where


  -- | Returns list of supported file extensions for this template type.
  templateExtensions :: NonEmpty Text
                     -- ^ list of supported file extensions


  -- | Parses template from given raw text.
  parseTemplate :: MonadThrow m
                => Maybe Text
                -- ^ name of the template (optional)
                -> Text
                -- ^ raw template text
                -> m t
                -- ^ parsed template


  -- | Renders parsed template and replaces all variables with actual values.
  renderTemplate :: MonadThrow m
                 => HashMap Text Text
                 -- ^ values of variables to replace
                 -> t
                 -- ^ parsed template to render
                 -> m Text
                 -- ^ rendered template text
