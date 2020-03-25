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
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
module Headroom.Template
  ( Template(..)
  , TemplateError(..)
  , loadTemplate
  , templateErrorMessage
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

-- | Error during processing of template
data TemplateError
  = MissingVariables Text [Text] -- ^ not all variables were filled in template
  | ParseError Text              -- ^ error parsing template file
  deriving Show

-- | Loads and parses template from file.
loadTemplate :: (MonadIO m, Template t)
             => FilePath -- ^ path to template file
             -> m t      -- ^ loaded and parsed template
loadTemplate path = do
  raw <- loadFile path
  liftIO $ parseTemplate (Just $ T.pack path) raw

-- | User-friendly description of the given error.
templateErrorMessage :: TemplateError -- ^ error to get message for
                     -> Text          -- ^ error message
templateErrorMessage = \case
  MissingVariables name variables -> missingVariables name variables
  ParseError msg                  -> parseError msg
 where
  missingVariables name variables = mconcat
    ["Missing variables for template '", name, "': ", T.pack . show $ variables]
  parseError msg = "Error parsing template: " <> msg

