{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Headroom.Header.Impl.CSS
  ( headerSizeCSS
  )
where

import           Headroom.Header.Utils          ( reML )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Heavy


-- | Returns size of license header (as number of lines) in given /CSS/ source
-- code. The very first comment block is considered as license header, anything
-- after as start of the actual code.
headerSizeCSS :: T.Text -> Int
headerSizeCSS text =
  case L.headMaybe $ scan [reML|(\/\*(?:.*?)\*\/)\s*|(\s*)|] text of
    Just (comment, _) -> L.length . T.lines $ comment
    _                 -> 0
