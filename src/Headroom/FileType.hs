{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Headroom.FileType
  ( configByFileType
  , fileTypeByExt
  , listExtensions
  )
where

import           Headroom.Types                 ( FileType(..)
                                                , HeaderConfig(..)
                                                , HeadersConfig(..)
                                                )
import           Headroom.Types.EnumExtra       ( EnumExtra(..) )
import           RIO
import qualified RIO.List                      as L


-- | Returns 'FileType' for given file extension (without dot), using configured
-- values from the 'HeadersConfig'.
fileTypeByExt :: HeadersConfig  -- ^ license headers configuration
              -> Text           -- ^ file extension (without dot)
              -> Maybe FileType -- ^ found 'FileType'
fileTypeByExt config ext =
  L.find (elem ext . listExtensions config) (allValues @FileType)


-- | Lists all recognized file extensions for given 'FileType', using configured
-- values from the 'HeadersConfig'.
listExtensions :: HeadersConfig -- ^ license headers configuration
               -> FileType      -- ^ 'FileType' for which to list extensions
               -> [Text]        -- ^ list of appropriate file extensions
listExtensions config fileType =
  hcFileExtensions (configByFileType config fileType)


-- | Returns the proper 'HeaderConfig' for the given 'FileType', selected
-- from the 'HeadersConfig'.
configByFileType :: HeadersConfig -- ^ license headers configuration
                 -> FileType      -- ^ selected 'FileType'
                 -> HeaderConfig  -- ^ appropriate 'HeaderConfig'
configByFileType HeadersConfig {..} fileType = case fileType of
  CSS     -> hscCss
  Haskell -> hscHaskell
  HTML    -> hscHtml
  Java    -> hscJava
  JS      -> hscJs
  Rust    -> hscRust
  Scala   -> hscScala
