{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Text
  ( detectNewLine
  , showNewLine
  , lines'
  , unlines'
  )
where

import           Headroom.Types                 ( NewLine(..) )
import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP


detectNewLine :: T.Text -> Maybe NewLine
detectNewLine text | showNewLine CRLF `T.isInfixOf` text = Just CRLF
                   | showNewLine CR `T.isInfixOf` text   = Just CR
                   | showNewLine LF `T.isInfixOf` text   = Just LF
                   | otherwise                           = Nothing

showNewLine :: NewLine -> T.Text
showNewLine CR   = "\r"
showNewLine CRLF = "\r\n"
showNewLine LF   = "\n"

lines' :: T.Text -> (NewLine, [T.Text])
lines' text = (newLine, chunks)
 where
  newLine = fromMaybe LF (detectNewLine text)
  chunks  = TP.splitOn (showNewLine newLine) text

unlines' :: NewLine -> [T.Text] -> T.Text
unlines' newLine = T.intercalate $ showNewLine newLine
