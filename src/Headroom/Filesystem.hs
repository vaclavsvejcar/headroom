{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Filesystem
  ( findFiles
  , findFilesByExts
  , listFiles
  )
where

import           Control.Monad                  ( forM )
import           RIO
import           RIO.Directory                  ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           RIO.FilePath                   ( (</>)
                                                , isExtensionOf
                                                )
import qualified RIO.Text                      as T


findFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFiles path predicate = fmap (filter predicate) (listFiles path)

findFilesByExts :: FilePath -> [T.Text] -> IO [FilePath]
findFilesByExts path exts = findFiles path predicate
  where predicate p = any (`isExtensionOf` p) (fmap T.unpack exts)

listFiles :: FilePath -> IO [FilePath]
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
