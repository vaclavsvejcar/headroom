{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Header.HTML
  ( htmlHeaderConfig
  )
where

import           Headroom.Types                 ( PartialHeaderConfig(..) )
import           RIO

htmlHeaderConfig :: PartialHeaderConfig
htmlHeaderConfig = mempty { phcFileExtensions = pure ["htm", "html"]
                          , phcPutAfter       = pure []
                          , phcStartsWith     = pure "<!--"
                          , phcEndsWith       = pure "-->"
                          }
