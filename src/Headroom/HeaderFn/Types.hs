{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.HeaderFn.Types
Description : Data types for /license header functions/
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types and /type class/ instances for the
/license header functions/.
-}

module Headroom.HeaderFn.Types
  ( HeaderFn(..)
  )
where

import           RIO


-- | Definition of /license header function/, i.e. function, that is applied to
-- already rendered /license header/, performs some logic and returns modified
-- text of /license header/. Given that the /reader monad/ and 'ReaderT'
-- transformer is used, any configuration is provided using the @env@
-- environment. When combined with the "Headroom.Data.Has" monad, it provides
-- powerful way how to combine different /license header function/ and
-- environments.
--
-- = Structure of License Header Function Type
-- 
-- @
-- __Text -> ReaderT env Identity Text__
--   │               │            │
--   └─ rendered text of license header
--                   │            │
--                   └─ environment holding possible configuration
--                                │
--                                └─ modified license header text
-- @
newtype HeaderFn env = HeaderFn (Text -> ReaderT env Identity Text)

instance Semigroup (HeaderFn env) where
  HeaderFn fnX <> HeaderFn fnY = HeaderFn $ fnX >=> fnY

instance Monoid (HeaderFn env) where
  mempty = HeaderFn $ \input -> pure input
