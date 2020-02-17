{-|
Module      : MODULE_NAME
Description : Extras for text manipulation
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Adds some extra functionality to the "Data.Text" module.
-}
{-# LANGUAGE LambdaCase        #-}
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
import           RIO.Text                       ( isInfixOf )
import qualified RIO.Text                      as T
import qualified RIO.Text.Partial              as TP


-- | Detects which newline character is used in given text (if any).
--
-- >>> detectNewLine "foo\nbar"
-- Just LF
detectNewLine :: Text          -- ^ input text
              -> Maybe NewLine -- ^ detected newline character
detectNewLine text | showNewLine CRLF `isInfixOf` text = Just CRLF
                   | showNewLine CR `isInfixOf` text   = Just CR
                   | showNewLine LF `isInfixOf` text   = Just LF
                   | otherwise                         = Nothing

-- | Renders appropriate newline character (e.g. @\n@) for given 'NewLine'
-- representation.
--
-- >>> showNewLine LF
-- "\n"
showNewLine :: NewLine -- ^ newline character to render
            -> Text    -- ^ rendered character
showNewLine = \case
  CR   -> "\r"
  CRLF -> "\r\n"
  LF   -> "\n"

-- | Split text into lines, return lines and detected newline separator.
--
-- >>> lines' "foo\nbar"
-- (LF,["foo","bar"])
lines' :: Text              -- ^ text to split
       -> (NewLine, [Text]) -- ^ detected newline separator and split lines
lines' text = (newLine, chunks)
 where
  newLine = fromMaybe LF (detectNewLine text)
  chunks  = TP.splitOn (showNewLine newLine) text

-- | Join individual text lines into single text, using given newline separator.
--
-- >>> unlines' LF ["foo", "bar"]
-- "foo\nbar"
unlines' :: NewLine -> [Text] -> Text
unlines' newLine = T.intercalate $ showNewLine newLine
