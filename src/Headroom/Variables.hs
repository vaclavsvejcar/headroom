{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Headroom.Variables
Description : Support for template variables
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module containing costructor and useful functions for the 'Variables' data type.
-}

module Headroom.Variables
  ( -- * Constructing Variables
    mkVariables
  , dynamicVariables
    -- * Parsing Variables
  , parseVariables
    -- * Processing Variables
  , compileVariables
  )
where

import           RIO

import           Data.Time.Calendar             ( toGregorian )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.LocalTime            ( getCurrentTimeZone
                                                , localDay
                                                , utcToLocalTime
                                                )
import           Headroom.Meta                  ( TemplateType )
import           Headroom.Template              ( Template(..) )
import           Headroom.Types                 ( ApplicationError(..)
                                                , ConfigurationError(..)
                                                , Variables(..)
                                                )
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T


-- | Constructor function for 'Variables' data type.
--
-- >>> mkVariables [("key1", "value1")]
-- Variables {unVariables = fromList [("key1","value1")]}
mkVariables :: [(Text, Text)]
            -- ^ pairs of /key-value/
            -> Variables
            -- ^ constructed variables
mkVariables = Variables . HM.fromList


-- | /Dynamic variables/ that are common for all parsed files.
--
-- * @___current_year__@ - current year
dynamicVariables :: MonadIO m => m Variables
dynamicVariables = do
  now      <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow      = utcToLocalTime timezone now
      (year, _, _) = toGregorian $ localDay zoneNow
  pure . mkVariables $ [("_current_year", tshow year)]


-- | Parses variables from raw input in @key=value@ format.
--
-- >>> parseVariables ["key1=value1"]
-- Variables {unVariables = fromList [("key1","value1")]}
parseVariables :: MonadThrow m
               => [Text]
               -- ^ list of raw variables
               -> m Variables
               -- ^ parsed variables
parseVariables variables = fmap mkVariables (mapM parse variables)
 where
  parse input = case T.split (== '=') input of
    [key, value] -> pure (key, value)
    _            -> throwM $ ConfigurationError (InvalidVariable input)


-- | Compiles variable values that are itself mini-templates, where their
-- variables will be substituted by other variable values (if possible).
-- Note that recursive variable reference and/or cyclic references are not
-- supported.
--
-- >>> compileVariables $ mkVariables [("name", "John"), ("msg", "Hello, {{ name }}")]
-- Variables {unVariables = fromList [("msg","Hello, John"),("name","John")]}
compileVariables :: (MonadThrow m)
                 => Variables
                 -- ^ input variables to compile
                 -> m Variables
                 -- ^ compiled variables
compileVariables variables@(Variables kvs) = do
  compiled <- mapM compileVariable (HM.toList kvs)
  pure $ mkVariables compiled
 where
  compileVariable (key, value) = do
    parsed   <- parseTemplate @TemplateType (Just $ "variable " <> key) value
    rendered <- renderTemplate variables parsed
    pure (key, rendered)