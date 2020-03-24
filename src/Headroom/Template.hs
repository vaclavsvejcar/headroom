{-|
Module      : Headroom.Template
Description : Generic support for license header templates
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Provides generic support for the license header templates, represented by the
'Template' type class. Various implementations can be plugged in by creating
custom instance of this type class.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}
module Headroom.Template
  ( Template(..)
  , loadTemplate
  )
where

import           Headroom.FileSystem            ( loadFile )
import           RIO
import qualified RIO.Text                      as T


-- | Type class representing generic license header template support.
class Template t where
  -- | Returns list of supported file extensions for this template type.
  templateExtensions :: NonEmpty Text -- ^ list of supported file extensions

  -- | Parses template from given raw text.
  parseTemplate :: MonadThrow m
                => Maybe Text -- ^ name of the template (optional)
                -> Text       -- ^ raw template text
                -> m t        -- ^ parsed template

  -- | Renders parsed template and replaces all variables.
  renderTemplate :: MonadThrow m
                => HashMap Text Text    -- ^ variables to replace
                -> t                    -- ^ parsed template to render
                -> m Text               -- ^ rendered template text

-- | Loads and parses template from file.
loadTemplate :: (MonadIO m, Template t)
             => FilePath -- ^ path to template file
             -> m t      -- ^ loaded and parsed template
loadTemplate path = do
  raw <- loadFile path
  liftIO $ parseTemplate (Just $ T.pack path) raw

