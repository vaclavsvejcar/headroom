{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Headroom.HeaderFn
Description : Support for /license header functions/
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

/License header functions/ are basically functions that allows to post-process
already rendered /license headers/. This is useful to perform some additional
operations such as some sort of text alignment, update some parts of the header,
etc.
-}

module Headroom.HeaderFn
  ( runHeaderFn
  )
where

import           Headroom.HeaderFn.Types        ( HeaderFn(..) )
import           RIO

-- | Runs the /license header function/ using the given /environment/ and text
-- of rendered /license header/ as input.
runHeaderFn :: HeaderFn env
            -- ^ /license header function/ to run
            -> env
            -- ^ environment value
            -> Text
            -- ^ text of rendered /license header/
            -> Text
            -- ^ processed text of /license header/
runHeaderFn (HeaderFn fn) env input = runIdentity $ runReaderT (fn input) env
