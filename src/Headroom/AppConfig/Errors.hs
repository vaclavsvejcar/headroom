{-|
Module      : Headroom.AppConfig.Errors
Description : Application config error data types
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Data types representing errors occuring during processing of application
configuration.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.AppConfig.Errors
  ( AppConfigError(..)
  , ValidationError(..)
  , appConfigErrorMessage
  )
where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T

-- | Error that might occur during processing of application configuration.
data AppConfigError
  = ValidationFailed [ValidationError] -- ^ validation of application config failed
  | InvalidVariable Text               -- ^ error parsing variable from input
  deriving (Show)

-- | Error that might occur during validation of application config
data ValidationError
  = EmptySourcePaths   -- ^ no paths to source code files provided
  | EmptyTemplatePaths -- ^ no paths to license header templates provided
  deriving (Show)

-- | User-friendly description of the given error.
appConfigErrorMessage :: AppConfigError -- ^ error to get message for
                      -> Text           -- ^ error message
appConfigErrorMessage = \case
  InvalidVariable  tried  -> invalidVariable tried
  ValidationFailed errors -> invalidAppConfig errors

invalidAppConfig :: [ValidationError] -> Text
invalidAppConfig errors = T.pack $ mconcat
  [ "Invalid configuration, following problems found:\n"
  , L.intercalate
    "\n"
    (fmap (\e -> "\t- " <> (T.unpack . errorMessage $ e)) errors)
  ]
 where
  errorMessage = \case
    EmptySourcePaths   -> "no paths to source code files"
    EmptyTemplatePaths -> "no paths to template files"

invalidVariable :: Text -> Text
invalidVariable tried = "Cannot parse variable key=value from: " <> tried
