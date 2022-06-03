{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Variables
-- Description : Support for template variables
-- Copyright   : (c) 2019-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module containing costructor and useful functions for the 'Variables' data type.
module Headroom.Variables (
    -- * Constructing Variables
      mkVariables
    , dynamicVariables

      -- * Parsing Variables
    , parseVariables

      -- * Processing Variables
    , compileVariables
) where

import Data.String.Interpolate (iii)
import Headroom.Template (Template (..))
import Headroom.Template.TemplateRef (TemplateRef (..))
import Headroom.Types (
    CurrentYear (..)
    , fromHeadroomError
    , toHeadroomError
 )
import Headroom.Variables.Types (Variables (..))
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T

-- | Constructor function for 'Variables' data type.
--
-- >>> mkVariables [("key1", "value1")]
-- Variables (fromList [("key1","value1")])
mkVariables ::
    -- | pairs of /key-value/
    [(Text, Text)] ->
    -- | constructed variables
    Variables
mkVariables = Variables . HM.fromList

-- | /Dynamic variables/ that are common for all parsed files.
--
-- * @___current_year__@ - current year
dynamicVariables ::
    -- | current year
    CurrentYear ->
    -- | map of /dynamic variables/
    Variables
dynamicVariables (CurrentYear year) =
    mkVariables [("_current_year", tshow year)]

-- | Parses variables from raw input in @key=value@ format.
--
-- >>> parseVariables ["key1=value1"]
-- Variables (fromList [("key1","value1")])
parseVariables ::
    MonadThrow m =>
    -- | list of raw variables
    [Text] ->
    -- | parsed variables
    m Variables
parseVariables variables = fmap mkVariables (mapM parse variables)
  where
    parse input = case T.split (== '=') input of
        [key, value] -> pure (key, value)
        _ -> throwM $ InvalidVariable input

-- | Compiles variable values that are itself mini-templates, where their
-- variables will be substituted by other variable values (if possible).
-- Note that recursive variable reference and/or cyclic references are not
-- supported.
--
-- >>> import Headroom.Template.Mustache (Mustache)
-- >>> let compiled = compileVariables @Mustache $ mkVariables [("name", "John"), ("msg", "Hello, {{ name }}")]
-- >>> let expected = mkVariables [("name", "John"), ("msg", "Hello, John")]
-- >>> compiled == Just expected
-- True
compileVariables ::
    forall a m.
    (Template a, MonadThrow m) =>
    -- | input variables to compile
    Variables ->
    -- | compiled variables
    m Variables
compileVariables variables@(Variables kvs) = do
    compiled <- mapM compileVariable (HM.toList kvs)
    pure $ mkVariables compiled
  where
    compileVariable (key, value) = do
        parsed <- parseTemplate @a (InlineRef value) value
        rendered <- renderTemplate variables parsed
        pure (key, rendered)

---------------------------------  Error Types  --------------------------------

-- | Exception specific to the "Headroom.Variables" module.
data VariablesError
    = -- | invalid variable input (as @key=value@)
      InvalidVariable Text
    deriving (Eq, Show)

instance Exception VariablesError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: VariablesError -> String
displayException' = \case
    InvalidVariable raw ->
        [iii|
      Cannot parse variable in format KEY=VALUE from: #{raw}
    |]
