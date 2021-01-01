{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

{-|
Module      : Headroom.Template.Mustache
Description : Implementation of /Mustache/ template support
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module provides support for <https://mustache.github.io Mustache>
templates.
-}

module Headroom.Template.Mustache
  ( Mustache(..)
  )
where

import           Headroom.Template                   ( Template(..)
                                                     , TemplateError(..)
                                                     )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import qualified RIO.Text                           as T
import qualified Text.Mustache                      as MU
import           Text.Mustache.Render                ( SubstitutionError(..) )


-- | The /Mustache/ template.
data Mustache = Mustache
  { mCompiledTemplate :: MU.Template
  , mRawTemplate      :: Text
  }
  deriving Show

instance Eq Mustache where
  a == b = mRawTemplate a == mRawTemplate b


-- | Support for /Mustache/ templates.
instance Template Mustache where
  templateExtensions = "mustache" :| []
  parseTemplate      = parseTemplate'
  renderTemplate     = renderTemplate'
  rawTemplate        = mRawTemplate


parseTemplate' :: MonadThrow m => Maybe Text -> Text -> m Mustache
parseTemplate' name raw = case MU.compileTemplate templateName raw of
  Left  err -> throwM . ParseError $ tshow err
  Right res -> pure $ Mustache res raw
  where templateName = T.unpack . fromMaybe T.empty $ name


renderTemplate' :: MonadThrow m => Variables -> Mustache -> m Text
renderTemplate' (Variables variables) (Mustache t@(MU.Template name _ _) _) =
  case MU.checkedSubstitute t variables of
    ([], rendered) -> pure rendered
    (errs, rendered) ->
      let errs'            = missingVariables errs
          missingVariables = concatMap $ \case
            (VariableNotFound ps) -> ps
            _                     -> []
      in  if length errs == length errs'
            then throwM $ MissingVariables (T.pack name) errs'
            else pure rendered
