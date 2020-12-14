{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  )
where

import           Headroom.Configuration.Types        ( CtHeaderFnConfigs
                                                     , HeaderFnConfig(..)
                                                     , HeaderFnConfigs(..)
                                                     , UpdateCopyrightConfig(..)
                                                     )
import           Headroom.Data.Has                   ( Has(..) )
import           Headroom.Data.Lens                  ( suffixLenses
                                                     , suffixLensesFor
                                                     )
import           Headroom.HeaderFn.Types             ( HeaderFn(..) )
import           Headroom.HeaderFn.UpdateCopyright   ( SelectedAuthors(..)
                                                     , UpdateCopyrightMode(..)
                                                     , updateCopyright
                                                     )
import           Headroom.Meta                       ( TemplateType )
import           Headroom.TemplateSupport            ( TemplateSupport(..) )
import           Headroom.Types                      ( CurrentYear(..) )
import           Headroom.Variables.Types            ( Variables(..) )
import           Lens.Micro                          ( traverseOf )
import           RIO


suffixLenses ''HeaderFnConfigs
suffixLenses ''UpdateCopyrightConfig
suffixLensesFor ["hfcConfig"] ''HeaderFnConfig


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
  [ifEnabled hfcsUpdateCopyright updateCopyright]
 where
  ifEnabled HeaderFnConfig {..} fn | hfcEnabled = fn
                                   | otherwise  = mempty


-- | Takes already rendered /license header/ and post-process it based on the
-- given configuration.
postProcessHeader :: ConfiguredEnv
                  -- ^ configuration used to define post-processing behaviour
                  -> Text
                  -- ^ rendered text of /license header/
                  -> Text
                  -- ^ post-processed text of /license header/
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

suffixLensesFor ["ceCurrentYear", "ceUpdateCopyrightMode"] ''ConfiguredEnv

instance Has CurrentYear ConfiguredEnv where
  hasLens = ceCurrentYearL

instance Has UpdateCopyrightMode ConfiguredEnv where
  hasLens = ceUpdateCopyrightModeL


-- | Constructor function for 'ConfiguredEnv' data type. This function takes
-- 'Variables' as argument, because it performs template compilation on
-- selected fields of 'CtHeaderFnConfigs'.
mkConfiguredEnv :: (MonadThrow m)
                => CurrentYear
                -- ^ current year
                -> Variables
                -- ^ template variables
                -> CtHeaderFnConfigs
                -- ^ configuration of /license header functions/
                -> m ConfiguredEnv
                -- ^ environment data type
mkConfiguredEnv ceCurrentYear vars configs = do
  ceHeaderFnConfigs <- compileTemplates vars configs
  let ceUpdateCopyrightMode = mode ceHeaderFnConfigs
  pure ConfiguredEnv { .. }
 where
  authorsL = hfcsUpdateCopyrightL . hfcConfigL . uccSelectedAuthorsL
  mode configs' = maybe UpdateAllAuthors
                        (UpdateSelectedAuthors . SelectedAuthors)
                        (configs' ^. authorsL)


------------------------------  PRIVATE FUNCTIONS  -----------------------------

compileTemplates :: (MonadThrow m)
                 => Variables
                 -> CtHeaderFnConfigs
                 -> m CtHeaderFnConfigs
compileTemplates vars configs = configs & traverseOf authorsL compileAuthors'
 where
  authorsL        = hfcsUpdateCopyrightL . hfcConfigL . uccSelectedAuthorsL
  compileAuthors' = mapM . mapM $ compileAuthor
  compileAuthor author = do
    parsed <- parseTemplate @TemplateType (Just $ "author " <> author) author
    renderTemplate vars parsed




