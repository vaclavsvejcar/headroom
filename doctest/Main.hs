module Main (main) where

import Test.DocTest
    
main :: IO ()
main = doctest ["-XOverloadedStrings", "-XTemplateHaskell", "src"]