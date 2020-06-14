{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

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
  , configuredHeaderFn
  , postProcessHeader
    -- * Environment Data Types
  , ConfiguredEnv(..)
  , mkConfiguredEnv
    -- * Lenses
  , ceCurrentYearL
  , ceHeaderFnConfigsL
  , ceUpdateCopyrightModeL
  )
where

import           Headroom.Configuration.Types   ( CtHeaderFnConfigs
                                                , HeaderFnConfig(..)
                                                , HeaderFnConfigs(..)
                                                , hfcConfigL
                                                , hfcsUpdateCopyrightL
                                                , uccSelectedAuthorsL
                                                )
import           Headroom.Data.Has              ( Has(..) )
import           Headroom.Data.Lens             ( suffixLenses )
import           Headroom.HeaderFn.Types        ( HeaderFn(..) )
import           Headroom.HeaderFn.UpdateCopyright
                                                ( SelectedAuthors(..)
                                                , UpdateCopyrightMode(..)
                                                , updateCopyright
                                                )
import           Headroom.Types                 ( CurrentYear(..) )
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
runHeaderFn (HeaderFn fn) env input = runReader (fn input) env


-- | Composition of various /license header functions/, which environment is
-- based on /YAML/ configuration and which can be enabled/disabled to fit
-- end user's needs.
configuredHeaderFn :: (Has CurrentYear env, Has UpdateCopyrightMode env)
                   => CtHeaderFnConfigs
                   -- ^ configuration of /license header functions/
                   -> HeaderFn env
                   -- ^ composed /license header function/
configuredHeaderFn HeaderFnConfigs {..} = mconcat
  [condFn hfcsUpdateCopyright updateCopyright]
 where
  condFn HeaderFnConfig {..} fn | hfcEnabled = fn
                                | otherwise  = mempty


-- | Takes already rendered /license header/ and postprocess it based on the
-- given configuration.
postProcessHeader :: ConfiguredEnv
                  -- ^ configuration used to define postprocessing behaviour
                  -> Text
                  -- ^ rendered text of /license header/
                  -> Text
                  -- ^ postprocessed text of /license header/
postProcessHeader env = runHeaderFn (configuredHeaderFn configs) env
  where configs = ceHeaderFnConfigs env


-- | Environemnt data type for the composed /license header function/
-- ('configuredHeaderFn').
data ConfiguredEnv = ConfiguredEnv
  { ceCurrentYear         :: !CurrentYear
  -- ^ current year
  , ceHeaderFnConfigs     :: !CtHeaderFnConfigs
  -- ^ configuration of /license header functions/
  , ceUpdateCopyrightMode :: !UpdateCopyrightMode
  -- ^ mode used by the 'updateCopyright' /license header function/
  }
  deriving (Eq, Show)

suffixLenses ''ConfiguredEnv


-- | Constructor function for 'ConfiguredEnv' data type.
mkConfiguredEnv :: CurrentYear
                -- ^ current year
                -> CtHeaderFnConfigs
                -- ^ configuration of /license header functions/
                -> ConfiguredEnv
                -- ^ environment data type
mkConfiguredEnv ceCurrentYear ceHeaderFnConfigs = ConfiguredEnv { .. }
 where
  authorsL = hfcsUpdateCopyrightL . hfcConfigL . uccSelectedAuthorsL
  ceUpdateCopyrightMode = maybe UpdateAllAuthors
                                (UpdateSelectedAuthors . SelectedAuthors)
                                (ceHeaderFnConfigs ^. authorsL)


instance Has CurrentYear ConfiguredEnv where
  hasLens = ceCurrentYearL

instance Has UpdateCopyrightMode ConfiguredEnv where
  hasLens = ceUpdateCopyrightModeL



