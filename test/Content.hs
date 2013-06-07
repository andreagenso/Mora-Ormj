module Content(recursiveContentsScanner, recursiveContentsParser) where

import Control.Monad(forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), splitExtension)
import Mora.Ormj.Scanner
import Mora.Ormj.Parser
import Mora.Ormj.Scanner.Position
import UU.Scanner.Position
import UU.Parsing

getRecursiveContents :: FilePath -> IO[FilePath]
getRecursiveContents topdir = do
        names <- getDirectoryContents topdir
        let properNames = filter (`notElem` [".",".."]) names
        paths <- forM properNames $ \name -> do
          let path = topdir </> name
          isDirectory <- doesDirectoryExist path
          if isDirectory
                then getRecursiveContents path
                else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
                        then return [path]
                        else return []
        return (concat paths)


recursiveContentsScanner topdir = do
        names <- getDirectoryContents topdir
        let properNames = filter (`notElem` [".",".."]) names
        paths <- forM properNames $ \name -> do
          let path = topdir </> name
          isDirectory <- doesDirectoryExist path
          if isDirectory
                then recursiveContentsScanner  path
                else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
                        then return [scannerStr path]
                        else return []
        return (concat paths)


recursiveContentsParser topdir = do
        names <- getDirectoryContents topdir
        let properNames = filter (`notElem` [".","..", "Scanner"]) names
        paths <- forM properNames $ \name -> do
          let path = topdir </> name
          isDirectory <- doesDirectoryExist path
          if isDirectory
                then recursiveContentsParser  path
                else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
                        then
                            do
                                let sel = scannerStr path
                                res <- parseIO pOrmj sel
                                return [show res]
                        else    return []
        return (concat paths)