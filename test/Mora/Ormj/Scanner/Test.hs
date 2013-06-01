module Mora.Ormj.Scanner.Test(testSingleScanner, testAllTypeScanner, testScanner) where

import Control.Monad(forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), splitExtension)
import Mora.Ormj.Scanner


-- getRecursiveContents :: FilePath -> IO[FilePath]
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


getRecursiveContentsScanner topdir = do
        names <- getDirectoryContents topdir
        let properNames = filter (`notElem` [".",".."]) names
        paths <- forM properNames $ \name -> do
          let path = topdir </> name
          isDirectory <- doesDirectoryExist path
          if isDirectory
                then getRecursiveContentsScanner  path
                else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
                        then return [scannerStr path]
                        else return []
        return (concat paths)



testSingleScanner = scanner "/home/andrea/workspaceclipse_haskell/Mora-Ormj/test/Mora/Ormj/java/openjdk-6-src-b27/jdk/src/share/classes/com/sun/demo/jvmti/hprof/Tracker.java"

testAllTypeScanner = do
                       res <- getRecursiveContentsScanner "/home/andrea/workspaceclipse_haskell/Mora-Ormj/test/Mora/Ormj/Scanner"
                       let result = res
                       putStrLn (show result)

testScanner = do
                res <- getRecursiveContentsScanner "/home/andrea/workspaceclipse_haskell/Mora-Ormj/test"
                let result = res
                putStrLn (show result)