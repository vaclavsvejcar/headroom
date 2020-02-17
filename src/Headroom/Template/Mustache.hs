{-|
Module      : Headroom.Template.Mustache
Description : Support for Mustache templates
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Provides support for <https://mustache.github.io Mustache> templates using the
'Template' type class.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Template.Mustache
  ( Mustache(..)
  )
where

import           Headroom.Template              ( Template(..) )
import           RIO
import qualified Text.Mustache                 as MU
import           Text.Mustache.Render           ( SubstitutionError(..) )

import           Headroom.Types                 ( HeadroomError(..) )
import qualified RIO.Text                      as T


-- | The /Mustache/ template.
newtype Mustache = Mustache MU.Template deriving (Show)

-- | Support for /Mustache/ templates.
instance Template Mustache where
  templateExtensions _ = ["mustache"]
  parseTemplate  = parseTemplate'
  renderTemplate = renderTemplate'

parseTemplate' :: MonadThrow m => Maybe Text -> Text -> m Mustache
parseTemplate' name raw = case MU.compileTemplate templateName raw of
  Left  err -> throwM $ ParseError (T.pack . show $ err)
  Right res -> return $ Mustache res
  where templateName = T.unpack . fromMaybe "" $ name

renderTemplate' :: MonadThrow m => HashMap Text Text -> Mustache -> m Text
renderTemplate' variables (Mustache t@(MU.Template name _ _)) =
  case MU.checkedSubstitute t variables of
    ([], rendered) -> return rendered
    (errs, rendered) ->
      let errs' = missingVariables errs
      in  if length errs == length errs'
            then throwM $ MissingVariables (T.pack name) errs'
            else return rendered
 where
  missingVariables = concatMap
    (\case
      (VariableNotFound ps) -> ps
      _                     -> []
    )

