{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Filesystem where

import           Control.Monad                  ( forM )
import           RIO
import           RIO.Directory                  ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           RIO.FilePath                   ( (</>) )


listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  names <- getDirectoryContents dir
  let filteredNames = filter (`notElem` [".", ".."]) names
  paths <- forM filteredNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then listFiles path else return [path]
  return $ concat paths
