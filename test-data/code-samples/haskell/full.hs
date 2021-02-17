
{-|
Module      : Test
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

long
description

== Code sample
@
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..), listRepo )

example :: IO [FilePath]
example = do
  repo <- scanRepo @Git "path/to/repo"
  listRepo repo
@
-}

{-# LANGUAGE OverloadedStrings #-}
module Test where

{- single line block comment -}

{-
multi line block comment
-}

foo :: String
foo = "Hello, world!"

-- line comment