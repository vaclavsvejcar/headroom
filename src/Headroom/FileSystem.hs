{-|
Module      : Headroom.FileSystem
Description : Files/directories manipulation
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Functions for manipulating files and directories.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.FileSystem
  ( findFiles
  , findFilesByExts
  , findFilesByTypes
  , listFiles
  , loadFile
  )
where

import           Control.Monad                  ( forM )
import           Headroom.FileType              ( FileType
                                                , listExtensions
                                                )
import           RIO
import           RIO.Directory                  ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           RIO.FilePath                   ( isExtensionOf
                                                , (</>)
                                                )
import           RIO.Text                       ( Text )
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
                 => FilePath     -- ^ path to search
                 -> [FileType]   -- ^ list of file types
                 -> m [FilePath] -- ^ list of found files
findFilesByTypes path types = findFilesByExts path (types >>= listExtensions)

-- | Recursively find all files on given path. If file reference is passed
-- instead of directory, such file path is returned.
listFiles :: MonadIO m
          => FilePath     -- ^ path to search
          -> m [FilePath] -- ^ list of found files
listFiles fileOrDir = do
  isDir <- doesDirectoryExist fileOrDir
  if isDir then listDirectory fileOrDir else return [fileOrDir]
 where
  listDirectory dir = do
    names <- getDirectoryContents dir
    let filteredNames = filter (`notElem` [".", ".."]) names
    paths <- forM filteredNames $ \name -> do
      let path = dir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory then listFiles path else return [path]
    return $ concat paths

-- | Loads file content in UTF8 encoding.
loadFile :: MonadIO m
         => FilePath -- ^ file path
         -> m Text   -- ^ file content
loadFile = readFileUtf8
