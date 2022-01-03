module Main
  ( main
  )
where

import           Test.DocTest

main :: IO ()
main = doctest
  [ "-XDeriveAnyClass"
  , "-XFlexibleContexts"
  , "-XOverloadedStrings"
  , "-XTemplateHaskell"
  , "-XTypeApplications"
  , "-XTypeFamilies"
  , "-XQuasiQuotes"
  , "src"
  ]
