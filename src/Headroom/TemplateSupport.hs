{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StrictData          #-}

{-|
Module      : Headroom.TemplateSupport
Description : Extensible templating support
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing interface for working with template files in extensible way.
Supported template is represented by the 'TemplateSupport' /type class/.
-}

module Headroom.TemplateSupport
  ( -- * Extendable Template Support
    TemplateSupport(..)
    -- * Error Data Types
  , TemplateError(..)
  )
where

import           Headroom.Types                 ( fromHeadroomError
                                                , toHeadroomError
                                                )
import           Headroom.Variables.Types       ( Variables(..) )
import           RIO
import qualified RIO.Text                      as T


-- | /Type class/ representing supported template file.
class TemplateSupport t where

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
                 => Variables
                 -- ^ values of variables to replace
                 -> t
                 -- ^ parsed template to render
                 -> m Text
                 -- ^ rendered template text


  -- | Returns the raw text of the template, same that has been parsed by
  -- 'parseTemplate' method.
  rawTemplate :: t
              -- ^ template for which to return raw template text
              -> Text
              -- ^ raw template text


---------------------------------  ERROR TYPES  --------------------------------

-- | Error during processing template.
data TemplateError
  = MissingVariables Text [Text]
  -- ^ missing variable values
  | ParseError Text
  -- ^ error parsing raw template text
  deriving (Eq, Show, Typeable)


instance Exception TemplateError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: TemplateError -> String
displayException' = T.unpack . \case
  MissingVariables name variables -> missingVariables name variables
  ParseError msg                  -> parseError msg
 where
  missingVariables name variables =
    mconcat ["Missing variables for '", name, "': ", T.pack $ show variables]
  parseError msg = "Error parsing template: " <> msg
