{-|
Module      : Headroom.FileSystem
Description : Files/directories manipulation
Copyright   : (c) 2019 Vaclav Svejcar
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
import qualified RIO.Text                      as T


findFiles :: MonadIO m => FilePath -> (FilePath -> Bool) -> m [FilePath]
findFiles path predicate = fmap (filter predicate) (listFiles path)

findFilesByExts :: MonadIO m => FilePath -> [T.Text] -> m [FilePath]
findFilesByExts path exts = findFiles path predicate
  where predicate p = any (`isExtensionOf` p) (fmap T.unpack exts)

findFilesByTypes :: MonadIO m => FilePath -> [FileType] -> m [FilePath]
findFilesByTypes path types = findFilesByExts path (types >>= listExtensions)

listFiles :: MonadIO m => FilePath -> m [FilePath]
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
