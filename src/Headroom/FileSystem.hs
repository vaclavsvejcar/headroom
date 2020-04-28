{-|
Module      : Headroom.FileSystem
Description : Operations related to files and file system
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing functions for working with the local file system, its file and
directories.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.FileSystem
  ( -- * Traversing the File System
    findFiles
  , findFilesByExts
  , findFilesByTypes
  , listFiles
  , loadFile
    -- * Working with Files/Directories
  , doesFileExist
  , getCurrentDirectory
  , createDirectory
    -- * Working with Files Metadata
  , fileExtension
    -- * Other
  , excludePaths
  )
where

import           Headroom.FileType              ( listExtensions )
import           Headroom.Regex                 ( compile'
                                                , joinPatterns
                                                , match'
                                                )
import           Headroom.Types                 ( FileType
                                                , HeadersConfig(..)
                                                )
import           RIO
import           RIO.Directory                  ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , getCurrentDirectory
                                                , getDirectoryContents
                                                )
import           RIO.FilePath                   ( isExtensionOf
                                                , takeExtension
                                                , (</>)
                                                )
import qualified RIO.List                      as L
import qualified RIO.Text                      as T



-- | Recursively finds files on given path whose filename matches the predicate.
findFiles :: MonadIO m
          => FilePath           -- ^ path to search
          -> (FilePath -> Bool) -- ^ predicate to match filename
          -> m [FilePath]       -- ^ found files
findFiles path predicate = fmap (filter predicate) (listFiles path)


-- | Recursively finds files on given path by file extensions.
findFilesByExts :: MonadIO m
                => FilePath     -- ^ path to search
                -> [Text]       -- ^ list of file extensions (without dot)
                -> m [FilePath] -- ^ list of found files
findFilesByExts path exts = findFiles path predicate
  where predicate p = any (`isExtensionOf` p) (fmap T.unpack exts)


-- | Recursively find files on given path by their file types.
findFilesByTypes :: MonadIO m
                 => HeadersConfig -- ^ configuration of license headers
                 -> [FileType]    -- ^ list of file types
                 -> FilePath      -- ^ path to search
                 -> m [FilePath]  -- ^ list of found files
findFilesByTypes headersConfig types path =
  findFilesByExts path (types >>= listExtensions headersConfig)


-- | Recursively find all files on given path. If file reference is passed
-- instead of directory, such file path is returned.
listFiles :: MonadIO m
          => FilePath     -- ^ path to search
          -> m [FilePath] -- ^ list of found files
listFiles fileOrDir = do
  isDir <- doesDirectoryExist fileOrDir
  if isDir then listDirectory fileOrDir else pure [fileOrDir]
 where
  listDirectory dir = do
    names <- getDirectoryContents dir
    let filteredNames = filter (`notElem` [".", ".."]) names
    paths <- forM filteredNames $ \name -> do
      let path = dir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory then listFiles path else pure [path]
    pure $ concat paths


-- | Returns file extension for given path (if file), or nothing otherwise.
--
-- >>> fileExtension "path/to/some/file.txt"
-- Just "txt"
fileExtension :: FilePath -> Maybe Text
fileExtension path = case takeExtension path of
  '.' : xs -> Just $ T.pack xs
  _        -> Nothing


-- | Loads file content in UTF8 encoding.
loadFile :: MonadIO m
         => FilePath -- ^ file path
         -> m Text   -- ^ file content
loadFile = readFileUtf8


-- | Takes list of patterns and file paths and returns list of file paths where
-- those matching the given patterns are excluded.
--
-- >>> excludePaths ["\\.hidden", "zzz"] ["foo/.hidden", "test/bar", "x/zzz/e"]
-- ["test/bar"]
excludePaths :: [Text]     -- ^ patterns describing paths to exclude
             -> [FilePath] -- ^ list of file paths
             -> [FilePath] -- ^ resulting list of file paths
excludePaths _        []    = []
excludePaths []       paths = paths
excludePaths patterns paths = go $ compile' <$> joinPatterns patterns
 where
  go Nothing      = paths
  go (Just regex) = L.filter (isNothing . match' regex . T.pack) paths
