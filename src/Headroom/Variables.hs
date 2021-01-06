{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Headroom.Variables
Description : Support for template variables
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module containing costructor and useful functions for the 'Variables' data type.
-}

module Headroom.Variables
  ( -- * Constructing Variables
    empty
  , mkVariables
  , dynamicVariables
    -- * Parsing Variables
  , parseVariables
    -- * Processing Variables
  , compileVariables
  )
where

import           Headroom.Meta                       ( TemplateType )
import           Headroom.Template                   ( Template(..) )
import           Headroom.Types                      ( CurrentYear(..)
                                                     , fromHeadroomError
                                                     , toHeadroomError
                                                     )
import           Headroom.Variables.Types            ( Variables(..) )
import           RIO
import qualified RIO.HashMap                        as HM
import qualified RIO.Text                           as T


-- | Empty variables.
empty :: Variables
empty = mkVariables []

-- | Constructor function for 'Variables' data type.
--
-- >>> mkVariables [("key1", "value1")]
-- Variables (fromList [("key1","value1")])
mkVariables :: [(Text, Text)]
            -- ^ pairs of /key-value/
            -> Variables
            -- ^ constructed variables
mkVariables = Variables . HM.fromList


-- | /Dynamic variables/ that are common for all parsed files.
--
-- * @___current_year__@ - current year
dynamicVariables :: CurrentYear
                 -- ^ current year
                 -> Variables
                 -- ^ map of /dynamic variables/
dynamicVariables (CurrentYear year) =
  mkVariables [("_current_year", tshow year)]


-- | Parses variables from raw input in @key=value@ format.
--
-- >>> parseVariables ["key1=value1"]
-- Variables (fromList [("key1","value1")])
parseVariables :: MonadThrow m
               => [Text]
               -- ^ list of raw variables
               -> m Variables
               -- ^ parsed variables
parseVariables variables = fmap mkVariables (mapM parse variables)
 where
  parse input = case T.split (== '=') input of
    [key, value] -> pure (key, value)
    _            -> throwM $ InvalidVariable input


-- | Compiles variable values that are itself mini-templates, where their
-- variables will be substituted by other variable values (if possible).
-- Note that recursive variable reference and/or cyclic references are not
-- supported.
--
-- >>> compileVariables $ mkVariables [("name", "John"), ("msg", "Hello, {{ name }}")]
-- Variables (fromList [("msg","Hello, John"),("name","John")])
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


---------------------------------  Error Types  --------------------------------

-- | Exception specific to the "Headroom.Variables" module.
data VariablesError = InvalidVariable Text
                    -- ^ invalid variable input (as @key=value@)
  deriving (Eq, Show)


instance Exception VariablesError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError


displayException' :: VariablesError -> String
displayException' = T.unpack . \case
  InvalidVariable raw -> ("Cannot parse variable key=value from: " <> raw)
