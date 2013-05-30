module Mora.Ormj.Scanner.Test(getRecursiveContents) where

import Control.Monad(forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), splitExtension)
import Scanner


getRecursiveContents :: FilePath -> IO[FilePath]

getRecursiveContents topdir = do
        names <- getDirectoryContents topdir
        let properNames = filter (`notElem` [".",".."]) names
        paths <- forM properNames $ \name -> do
          let path = topdir </> name
          isDirectory <- doesDirectoryExist path
          if isDirectory
                then getRecursiveContents path
                else if snd (splitExtension path) == ".java" 
                        then return [path]
                        else return []
        return (concat paths)                        