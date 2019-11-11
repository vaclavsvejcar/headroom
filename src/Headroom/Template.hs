module Headroom.Template
  ( strip
  )
where

import           Data.Char

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
