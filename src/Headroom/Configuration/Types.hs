{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Headroom.Configuration.Types
Description : Data types for /Headroom/ configuration
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains data types representing /Headroom/ configuration options.
Related logic is available in "Headroom.Configuration" module.

Data types related to /Headroom/ configuration uses the
<https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67 partial options monoid>
pattern, but instead of defining separate data type for each /phase/
(/partial/ or /complete/ configuration), the /phase/ is expressed by the 'Phase'
data type and related /closed type family/.
-}

module Headroom.Configuration.Types
  ( -- * Type Families
    Phase(..)
  , (:::)
    -- * Data Types
    -- ** Header Functions
  , UpdateCopyrightConfig
  , PtUpdateCopyrightConfig
  , UpdateCopyrightConfig'(..)
  , HeaderFnConfig
  , PtHeaderFnConfig
  , HeaderFnConfig'(..)
  , HeaderFnConfigs
  , PtHeaderFnConfigs
  , HeaderFnConfigs'(..)
    -- * Lenses
  , uccSelectedAuthorsL
  , hfcEnabledL
  , hfcConfigL
  , hfcsUpdateCopyrightL
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , withObject
                                                , (.!=)
                                                , (.:?)
                                                )
import           Data.Monoid                    ( Last(..) )
import           Headroom.Data.Lens             ( suffixLenses )
import           RIO


------------------------------------  Phase  -----------------------------------

-- | Data type representing state of given configuration data type.
data Phase
  = Partial
  -- ^ partial configuration, could be combined with another or validated to
  -- produce the complete configuration
  | Complete
  -- ^ complete configuration, result of combining and validation of partial
  -- configuration


-- | /Closed type family/ used to express the phase of given data type.
type family (p :: Phase) ::: a where
  'Partial  ::: a = Last a
  'Complete ::: a = a


----------------------------  UpdateCopyrightConfig  ---------------------------

-- | Main configuration for the "Headroom.HeaderFn.UpdateCopyright"
-- /license header function/.
data UpdateCopyrightConfig' (p :: Phase) = UpdateCopyrightConfig
  { uccSelectedAuthors :: !(p ::: Maybe (NonEmpty Text))
  -- ^ if specified, years will be updated only in copyright statements of
  -- given authors
  }

suffixLenses ''UpdateCopyrightConfig'

-- | Alias for complete variant of 'UpdateCopyrightConfig''.
type UpdateCopyrightConfig = UpdateCopyrightConfig' 'Complete

-- | Alias for partial variant of 'UpdateCopyrightConfig''.
type PtUpdateCopyrightConfig = UpdateCopyrightConfig' 'Partial

deriving instance Eq UpdateCopyrightConfig
deriving instance Eq PtUpdateCopyrightConfig
deriving instance Show UpdateCopyrightConfig
deriving instance Show PtUpdateCopyrightConfig

instance FromJSON PtUpdateCopyrightConfig where
  parseJSON = withObject "PtUpdateCopyrightConfig" $ \obj -> do
    uccSelectedAuthors <- Last <$> obj .:? "selected-authors-only"
    pure UpdateCopyrightConfig { .. }

instance Semigroup PtUpdateCopyrightConfig where
  x <> y = UpdateCopyrightConfig
    { uccSelectedAuthors = uccSelectedAuthors x <> uccSelectedAuthors y
    }

instance Monoid PtUpdateCopyrightConfig where
  mempty = UpdateCopyrightConfig mempty


-------------------------------  HeaderFnConfig  -------------------------------

-- | Configuration for selected /license header function/.
data HeaderFnConfig' (p :: Phase) c = HeaderFnConfig
  { hfcEnabled :: p ::: Bool
  -- ^ whether this function is enabled or not
  , hfcConfig  :: c p
  -- ^ custom configuration of the /license header function/
  }

suffixLenses ''HeaderFnConfig'

-- | Alias for complete variant of 'HeaderFnConfig''.
type HeaderFnConfig c = HeaderFnConfig' 'Complete c

-- | Alias for partial variant of 'HeaderFnConfig''.
type PtHeaderFnConfig c = HeaderFnConfig' 'Partial c


deriving instance (Eq (c 'Complete)) => Eq (HeaderFnConfig c)
deriving instance (Eq (c 'Partial)) => Eq (PtHeaderFnConfig c)
deriving instance (Show (c 'Complete)) => Show (HeaderFnConfig c)
deriving instance (Show (c 'Partial)) => Show (PtHeaderFnConfig c)

instance Semigroup (c 'Partial) => Semigroup (PtHeaderFnConfig c) where
  x <> y = HeaderFnConfig { hfcEnabled = hfcEnabled x <> hfcEnabled y
                          , hfcConfig  = hfcConfig x <> hfcConfig y
                          }

instance Monoid (c 'Partial) => Monoid (PtHeaderFnConfig c) where
  mempty = HeaderFnConfig mempty mempty

instance (FromJSON (c 'Partial), Monoid (c 'Partial)) => FromJSON (PtHeaderFnConfig c) where
  parseJSON = withObject "PtHeaderFnConfig" $ \obj -> do
    hfcEnabled <- Last <$> obj .:? "enabled"
    hfcConfig  <- obj .:? "config" .!= mempty
    pure HeaderFnConfig { .. }



-------------------------------  HeaderFnConfigs  ------------------------------

-- | Configuration of all known /license header functions/.
data HeaderFnConfigs' (p :: Phase) = HeaderFnConfigs
  { hfcsUpdateCopyright :: !(HeaderFnConfig' p UpdateCopyrightConfig')
  -- ^ configuration for the "Headroom.HeaderFn.UpdateCopyright"
  -- /license header function/
  }

suffixLenses ''HeaderFnConfigs'

-- | Alias for complete variant of 'HeaderFnConfigs''.
type HeaderFnConfigs = HeaderFnConfigs' 'Complete

-- | Alias for partial variant of 'HeaderFnConfigs''.
type PtHeaderFnConfigs = HeaderFnConfigs' 'Partial

deriving instance Eq HeaderFnConfigs
deriving instance Eq PtHeaderFnConfigs
deriving instance Show HeaderFnConfigs
deriving instance Show PtHeaderFnConfigs

instance Semigroup PtHeaderFnConfigs where
  x <> y = HeaderFnConfigs
    { hfcsUpdateCopyright = hfcsUpdateCopyright x <> hfcsUpdateCopyright y
    }

instance Monoid PtHeaderFnConfigs where
  mempty = HeaderFnConfigs mempty

instance FromJSON PtHeaderFnConfigs where
  parseJSON = withObject "PtHeaderFnConfigs" $ \obj -> do
    hfcsUpdateCopyright <- obj .:? "update-copyright" .!= mempty
    pure HeaderFnConfigs { .. }
