{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileSupport.Haskell
  ( extractModuleName
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )
import           Headroom.Regex                 ( compile'
                                                , match'
                                                )
import           RIO
import qualified RIO.Text                      as T
import           Text.Regex.PCRE.Light          ( Regex )


-- | Extracts name of /Haskell/ module from given source code file content.
--
-- >>> extractModuleName "{-# LANGUAGE OverloadedStrings #-}\nmodule Foo where"
-- Just "Foo"
extractModuleName :: Text
                  -- ^ input text
                  -> Maybe Text
                  -- ^ extracted module name
extractModuleName = go . T.lines
 where
  go []       = Nothing
  go (x : xs) = maybe (go xs) (^? element 1) (match' moduleNameRegex x)

moduleNameRegex :: Regex
moduleNameRegex = compile' "^module\\s+(\\S+)"
