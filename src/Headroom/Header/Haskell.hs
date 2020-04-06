{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.Haskell
  ( haskellHeaderConfig
  )
where

import           Headroom.Types                 ( PartialHeaderConfig(..) )
import           RIO

haskellHeaderConfig :: PartialHeaderConfig
haskellHeaderConfig = mempty { phcFileExtensions = pure ["hs"]
                             , phcPutAfter       = pure []
                             , phcStartsWith     = pure "{-"
                             , phcEndsWith       = pure "-}"
                             }
