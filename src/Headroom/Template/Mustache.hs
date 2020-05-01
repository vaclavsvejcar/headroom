{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Headroom.Template.Mustache
Description : Implementation of /Mustache/ template support
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides support for <https://mustache.github.io Mustache> templates.
-}

module Headroom.Template.Mustache
  ( Mustache(..)
  )
where

import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( ApplicationError(..)
                                                , TemplateError(..)
                                                )
import           RIO
import qualified RIO.Text                      as T
import qualified Text.Mustache                 as MU
import           Text.Mustache.Render           ( SubstitutionError(..) )


-- | The /Mustache/ template.
newtype Mustache = Mustache MU.Template deriving (Show)


-- | Support for /Mustache/ templates.
instance Template Mustache where
  templateExtensions = "mustache" :| []
  parseTemplate      = parseTemplate'
  renderTemplate     = renderTemplate'


parseTemplate' :: MonadThrow m => Maybe Text -> Text -> m Mustache
parseTemplate' name raw = case MU.compileTemplate templateName raw of
  Left  err -> throwM $ TemplateError (ParseError (T.pack . show $ err))
  Right res -> pure $ Mustache res
  where templateName = T.unpack . fromMaybe "" $ name


renderTemplate' :: MonadThrow m => HashMap Text Text -> Mustache -> m Text
renderTemplate' variables (Mustache t@(MU.Template name _ _)) =
  case MU.checkedSubstitute t variables of
    ([], rendered) -> pure rendered
    (errs, rendered) ->
      let errs' = missingVariables errs
      in  if length errs == length errs'
            then throwM $ TemplateError (MissingVariables (T.pack name) errs')
            else pure rendered
 where
  missingVariables = concatMap
    (\case
      (VariableNotFound ps) -> ps
      _                     -> []
    )
