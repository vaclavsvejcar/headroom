{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.IO.FileSystem
-- Description : File system related IO operations
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module providing functions for working with the local file system, its file and
-- directories.
module Headroom.IO.FileSystem
    ( -- * Type Aliases
      CreateDirectoryFn
    , DoesFileExistFn
    , FindFilesFn
    , FindFilesByExtsFn
    , FindFilesByTypesFn
    , GetCurrentDirectoryFn
    , GetUserDirectoryFn
    , ListFilesFn
    , LoadFileFn
    , WriteFileFn

      -- * Polymorphic Record
    , FileSystem (..)
    , mkFileSystem

      -- * Traversing the File System
    , findFiles
    , findFilesByExts
    , findFilesByTypes
    , listFiles
    , loadFile

      -- * Working with Files Metadata
    , fileExtension

      -- * Other
    , excludePaths
    )
where

import Headroom.Config.Types (CtHeadersConfig)
import Headroom.Data.Regex
    ( Regex
    , match
    )
import Headroom.FileType (listExtensions)
import Headroom.FileType.Types (FileType)
import RIO
import RIO.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , getDirectoryContents
    , getHomeDirectory
    )
import RIO.FilePath
    ( isExtensionOf
    , takeExtension
    , (</>)
    )
import qualified RIO.List as L
import qualified RIO.Text as T

--------------------------------  TYPE ALIASES  --------------------------------

-- | Type of a function that creates new empty directory on the given path.
type CreateDirectoryFn m =
    FilePath
    -- ^ path of new directory
    -> m ()
    -- ^ /IO/ action result

-- | Type of a function that returns 'True' if the argument file exists and is
-- not a directory, and 'False' otherwise.
type DoesFileExistFn m =
    FilePath
    -- ^ path to check
    -> m Bool
    -- ^ whether the given path is existing file

-- | Type of a function that recursively finds files on given path whose
-- filename matches the predicate.
type FindFilesFn m =
    FilePath
    -- ^ path to search
    -> (FilePath -> Bool)
    -- ^ predicate to match filename
    -> m [FilePath]
    -- ^ found files

-- | Type of a function that recursively finds files on given path by file
-- extensions.
type FindFilesByExtsFn m =
    FilePath
    -- ^ path to search
    -> [Text]
    -- ^ list of file extensions (without dot)
    -> m [FilePath]
    -- ^ list of found files

-- | Type of a function that recursively find files on given path by their
-- file types.
type FindFilesByTypesFn m =
    CtHeadersConfig
    -- ^ configuration of license headers
    -> [FileType]
    -- ^ list of file types
    -> FilePath
    -- ^ path to search
    -> m [FilePath]
    -- ^ list of found files

-- | Type of a function that obtains the current working directory as an
-- absolute path.
type GetCurrentDirectoryFn m = m FilePath

-- | Type of a function that obtains the user's home directory as an absolute
-- path.
type GetUserDirectoryFn m = m FilePath

-- | Type of a function that recursively find all files on given path. If file
-- reference is passed instead of directory, such file path is returned.
type ListFilesFn m =
    FilePath
    -- ^ path to search
    -> m [FilePath]
    -- ^ list of found files

-- | Type of a function that loads file content in UTF-8 encoding.
type LoadFileFn m =
    FilePath
    -- ^ file path
    -> m Text
    -- ^ file content

-- | Type of a function that writes file content in UTF-8 encoding.
type WriteFileFn m =
    FilePath
    -- ^ file path
    -> Text
    -- ^ file content
    -> m ()
    -- ^  write result

-----------------------------  POLYMORPHIC RECORD  -----------------------------

-- | /Polymorphic record/ composed of file system /IO/ function types, allowing
-- to abstract over concrete implementation. Whenever you need to use effectful
-- functions from this module, consider using this record instead of using them
-- directly, as it allows you to use different records for production code and
-- for testing, which is not as easy if you wire some of the provided functions
-- directly.
data FileSystem m = FileSystem
    { fsCreateDirectory :: CreateDirectoryFn m
    -- ^ Function that creates new empty directory on the given path.
    , fsDoesFileExist :: DoesFileExistFn m
    -- ^ Function that returns 'True' if the argument file exists and is not
    -- a directory, and 'False' otherwise.
    , fsFindFiles :: FindFilesFn m
    -- ^ Function that recursively finds files on given path whose filename
    -- matches the predicate.
    , fsFindFilesByExts :: FindFilesByExtsFn m
    -- ^ Function that recursively finds files on given path by file extensions.
    , fsFindFilesByTypes :: FindFilesByTypesFn m
    -- ^ Function that recursively find files on given path by their file types.
    , fsGetCurrentDirectory :: GetCurrentDirectoryFn m
    -- ^ Function that obtains the current working directory as an absolute path.
    , fsGetUserDirectory :: GetUserDirectoryFn m
    -- ^ Function that obtains the user's home directory as an absolute path.
    , fsListFiles :: ListFilesFn m
    -- ^ Function that recursively find all files on given path. If file reference
    -- is passed instead of directory, such file path is returned.
    , fsLoadFile :: LoadFileFn m
    -- ^ Function that loads file content in UTF-8 encoding.
    , fsWriteFile :: WriteFileFn m
    -- ^ Function that writes file content in UTF-8 encoding.
    }

-- | Creates new 'FileSystem' that performs actual disk /IO/ operations.
mkFileSystem :: (MonadIO m) => FileSystem m
mkFileSystem =
    FileSystem
        { fsCreateDirectory = createDirectoryIfMissing True
        , fsDoesFileExist = doesFileExist
        , fsFindFiles = findFiles
        , fsFindFilesByExts = findFilesByExts
        , fsFindFilesByTypes = findFilesByTypes
        , fsGetCurrentDirectory = getCurrentDirectory
        , fsGetUserDirectory = getHomeDirectory
        , fsListFiles = listFiles
        , fsLoadFile = loadFile
        , fsWriteFile = writeFileUtf8
        }

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Recursively finds files on given path whose filename matches the predicate.
findFiles :: (MonadIO m) => FindFilesFn m
findFiles path predicate = fmap (filter predicate) (listFiles path)

-- | Recursively finds files on given path by file extensions.
findFilesByExts :: (MonadIO m) => FindFilesByExtsFn m
findFilesByExts path exts = findFiles path predicate
  where
    predicate p = any (`isExtensionOf` p) (fmap T.unpack exts)

-- | Recursively find files on given path by their file types.
findFilesByTypes :: (MonadIO m) => FindFilesByTypesFn m
findFilesByTypes headersConfig types path =
    findFilesByExts path (types >>= listExtensions headersConfig)

-- | Recursively find all files on given path. If file reference is passed
-- instead of directory, such file path is returned.
listFiles :: (MonadIO m) => ListFilesFn m
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
fileExtension
    :: FilePath
    -- ^ path from which to extract file extension
    -> Maybe Text
    -- ^ extracted file extension
fileExtension (takeExtension -> '.' : xs) = Just $ T.pack xs
fileExtension _ = Nothing

-- | Loads file content in UTF8 encoding.
loadFile :: (MonadIO m) => LoadFileFn m
loadFile = readFileUtf8

-- | Takes list of patterns and file paths and returns list of file paths where
-- those matching the given patterns are excluded.
--
-- >>> import Headroom.Data.Regex (re)
-- >>> excludePaths [[re|\.hidden|], [re|zzz|]] ["foo/.hidden", "test/bar", "x/zzz/e"]
-- ["test/bar"]
excludePaths
    :: [Regex]
    -- ^ patterns describing paths to exclude
    -> [FilePath]
    -- ^ list of file paths
    -> [FilePath]
    -- ^ resulting list of file paths
excludePaths _ [] = []
excludePaths [] paths = paths
excludePaths patterns paths = L.filter excluded paths
  where
    excluded item = all (\p -> isNothing $ match p (T.pack item)) patterns
