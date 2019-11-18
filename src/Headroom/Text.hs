{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Text
  ( detectNewLine
  , showNewLine
  )
where

import           Headroom.Types                 ( NewLine(..) )
import           RIO
import qualified RIO.Text                      as T

detectNewLine :: T.Text -> Maybe NewLine
detectNewLine text | showNewLine CRLF `T.isInfixOf` text = Just CRLF
                   | showNewLine CR `T.isInfixOf` text   = Just CR
                   | showNewLine LF `T.isInfixOf` text   = Just LF
                   | otherwise                           = Nothing

showNewLine :: NewLine -> T.Text
showNewLine CR   = "\r"
showNewLine CRLF = "\r\n"
showNewLine LF   = "\n"
