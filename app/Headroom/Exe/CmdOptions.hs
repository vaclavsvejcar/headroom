{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Exe.CmdOptions
  ( CmdOptions(..)
  , def
  )
where

import           Headroom.Exe.OrphanInstances   ( )
import           RIO
import qualified RIO.Text                      as T
import           System.Console.CmdArgs

data CmdOptions =
  CmdOptions { foo :: T.Text
             , bar :: T.Text
             } deriving (Data, Show, Typeable)

instance Default CmdOptions where
  def =
    CmdOptions { foo = def &= help "test foo arg"
               , bar = def &= help "test bar arg"
               }
      &= summary "headroom, Copyright (c) 2019 Vaclav Svejcar"
      &= program "headroom"
