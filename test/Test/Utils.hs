{-# LANGUAGE NoImplicitPrelude #-}
module Test.Utils
  ( matchesException
  )
where

import           RIO

matchesException :: Exception e
                 => (Maybe e -> Bool)
                 -> Either SomeException r
                 -> Bool
matchesException cond (Left ex) | cond (fromException ex) = True
                                | otherwise               = False
matchesException _ _ = False
