{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.FileType
  ( fileTypeByExt
  , listExtensions
  )
where

import           Headroom.Types                 ( FileType(..) )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
import           RIO
import qualified RIO.List                      as L


-- | Returns 'FileType' for given file extension (without dot).
--
-- >>> fileTypeByExt "hs"
-- Just Haskell
fileTypeByExt :: Text           -- ^ file extension to search for
              -> Maybe FileType -- ^ corresponding 'FileType' (if found)
fileTypeByExt ext =
  L.find (elem ext . listExtensions) (allValues :: [FileType])

-- | Lists all recognized file extensions for given 'FileType'.
--
-- >>> listExtensions Haskell
-- ["hs"]
listExtensions :: FileType -- ^ 'FileType' to list extensions for
               -> [Text]   -- ^ list of found file extensions
listExtensions = \case
  CSS     -> ["css"]
  Haskell -> ["hs"]
  HTML    -> ["html", "htm"]
  Java    -> ["java"]
  JS      -> ["js"]
  Scala   -> ["scala"]
