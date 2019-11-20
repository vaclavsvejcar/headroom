{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Headroom.Exe.CmdOptions
import           RIO
import           System.Console.CmdArgs


main :: IO ()
main = do
  opts <- cmdArgsRun cmdOptions
  runSimpleApp . logInfo . displayShow $ "hello, " ++ show opts
