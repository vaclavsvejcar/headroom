{-# LANGUAGE NoImplicitPrelude #-}

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
  ( -- * Constructor Functions
    mkVariables
    -- * Parsing Variables
  , parseVariables
  )
where

import           RIO

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
