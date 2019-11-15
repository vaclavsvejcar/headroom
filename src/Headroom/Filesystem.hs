{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Filesystem where

import           Control.Monad                  ( forM )
import           RIO
import           RIO.Directory                  ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           RIO.FilePath                   ( (</>) )


traverseDir :: FilePath -> IO [FilePath]
traverseDir dir = do
  names <- getDirectoryContents dir
  let filteredNames = filter (`notElem` [".", ".."]) names
  paths <- forM filteredNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then traverseDir path else return [path]
  return $ concat paths
