{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.Header.Impl.CSS
  ( headerSizeCSS
  )
where

import           Headroom.Header.Utils          ( linesCountByRegex
                                                , reML
                                                )
import           RIO
import qualified RIO.Text                      as T


-- | Returns size of license header (as number of lines) in given /CSS/ source
-- code. The very first comment block is considered as license header, anything
-- after as start of the actual code.
headerSizeCSS :: T.Text -> Int
headerSizeCSS = linesCountByRegex [reML|(\/\*(?:.*?)\*\/)\s*|(\s*)|]
