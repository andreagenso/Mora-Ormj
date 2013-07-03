module Content(recursiveContentsScanner, recursiveContentsParser) where

import Control.Monad(forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), splitExtension)
import Control.Exception(evaluate)
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


-- recursiveContentsScanner topdir = do
--         names <- getDirectoryContents topdir
--         let properNames = filter (`notElem` [".",".."]) names
--         paths <- forM properNames $ \name -> do
--           let path = topdir </> name
--           isDirectory <- doesDirectoryExist path
--           if isDirectory
--                 then recursiveContentsScanner  path
--                 else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
--                         then
--                             do
--                             reading <- readFile path
--                             let tokens = classify reading (initPos path)
--                             return [tokens]
--                         else return []
--         return (concat paths)*/

-- recursiveContentsScanner :: IO DF2

-- recursiveContentsScanner :: FilePath -> IO[FilePath]
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
                                reading <- readFile path
                                let sel = classify reading (initPos path)
                                res <- parseIO pOrmj sel
                                return [show res]
                        else    return []
        return (concat paths)


recursiveContentsScanner topdir = do
        names <- getDirectoryContents topdir
        let properNames = filter (`notElem` [".",".."]) names
        paths <- forM properNames $ \name -> do
          let path = topdir </> name
          isDirectory <- doesDirectoryExist path
          if isDirectory
                then recursiveContentsScanner path
                else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
                        then return [path]
                        else return []
        return (concat paths)
