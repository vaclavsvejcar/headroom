{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Template
-- Description : Extensible templating support
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module providing interface for working with template files in extensible way.
-- Supported template is represented by the 'Template' /type class/.
module Headroom.Template (
    -- * Extendable Template Support
      Template (..)

      -- * Helper Functions
    , emptyTemplate

      -- * Error Data Types
    , TemplateError (..)
) where

import Data.String.Interpolate (iii)
import Headroom.Template.TemplateRef (TemplateRef (..))
import Headroom.Types (
    fromHeadroomError
    , toHeadroomError
 )
import Headroom.Variables.Types (Variables (..))
import RIO
import qualified RIO.Text as T

-- | /Type class/ representing supported template file.
class Template a where
    -- | Returns list of supported file extensions for this template type.
    templateExtensions ::
        -- | list of supported file extensions
        NonEmpty Text

    -- | Parses template from given raw text.
    parseTemplate ::
        MonadThrow m =>
        -- | reference to template source
        TemplateRef ->
        -- | raw template text
        Text ->
        -- | parsed template
        m a

    -- | Renders parsed template and replaces all variables with actual values.
    renderTemplate ::
        MonadThrow m =>
        -- | values of variables to replace
        Variables ->
        -- | parsed template to render
        a ->
        -- | rendered template text
        m Text

    -- | Returns the raw text of the template, same that has been parsed by
    -- 'parseTemplate' method.
    rawTemplate ::
        -- | template for which to return raw template text
        a ->
        -- | raw template text
        Text

    -- | Returns a reference to template source, from which this template was
    -- loaded.
    templateRef ::
        -- | template for which to return reference
        a ->
        -- | template reference
        TemplateRef

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Returns empty template of selected type.
emptyTemplate :: (MonadThrow m, Template a) => m a
emptyTemplate = parseTemplate (InlineRef T.empty) T.empty

---------------------------------  ERROR TYPES  --------------------------------

-- | Error during processing template.
data TemplateError
    = -- | missing variable values
      MissingVariables Text [Text]
    | -- | error parsing raw template text
      ParseError Text
    deriving (Eq, Show, Typeable)

instance Exception TemplateError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: TemplateError -> String
displayException' = \case
    MissingVariables name variables ->
        [iii|
      Missing variables for #{name}: #{variables}
    |]
    ParseError msg ->
        [iii|
      Error parsing template: #{msg}
    |]
