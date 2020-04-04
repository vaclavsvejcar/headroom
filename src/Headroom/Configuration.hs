{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Headroom.Configuration
  ( defaultPartialConfiguration
  , makeConfiguration
  )
where

import           Data.Monoid                    ( Last(..) )
import           Headroom.Types                 ( ApplicationError(..)
                                                , Configuration(..)
                                                , ConfigurationError(..)
                                                , PartialConfiguration(..)
                                                , RunMode(..)
                                                )
import           RIO
import qualified RIO.HashMap                   as HM


defaultPartialConfiguration :: PartialConfiguration
defaultPartialConfiguration =
  mempty { pcRunMode = pure Add, pcVariables = pure $ HM.fromList [] }

makeConfiguration :: MonadThrow m => PartialConfiguration -> m Configuration
makeConfiguration PartialConfiguration {..} = do
  cRunMode       <- lastOrError NoRunMode pcRunMode
  cSourcePaths   <- lastOrError NoSourcePaths pcSourcePaths
  cTemplatePaths <- lastOrError NoTemplatePaths pcTemplatePaths
  cVariables     <- lastOrError NoVariables pcVariables
  pure Configuration { .. }
 where
  lastOrError err (Last x) = maybe (throwM $ ConfigurationError err) pure x
