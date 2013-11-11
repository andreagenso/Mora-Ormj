module Content where

import Control.Monad(forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), splitExtension)
import Control.Exception(evaluate)
import Mora.Ormj.Scanner
import Mora.Ormj.Parser
import Mora.Ormj.Scanner.Position
import UU.Scanner.Position
import UU.Parsing

import Control.Monad (when, unless)
import Control.Proxy
import Control.Proxy.Safe hiding (readFileS)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import System.Directory (readable, getPermissions, doesDirectoryExist)
import System.FilePath ((</>), takeFileName)
import System.Posix (openDirStream, readDirStream, closeDirStream)
import System.IO (openFile, hClose, IOMode(ReadMode), hIsEOF)

import Mora.Ormj.Scanner.Token



-- --------------------------------------------------------------------------------------------------------------------
-- Pipe
contents
     :: (CheckP p)
     => FilePath -> () -> Producer (ExceptionP p) FilePath SafeIO ()
contents path () = do
     canRead <- tryIO $ fmap readable $ getPermissions path
     when canRead $ bracket id (openDirStream path) closeDirStream $ \dirp -> do
         let loop = do
                 file <- tryIO $ readDirStream dirp
                 case file of
                     [] -> return ()
                     _  -> do
                         respond (path </> file)
                         loop
         loop

contentsRecursive
     :: (CheckP p)
     => FilePath -> () -> Producer (ExceptionP p) FilePath SafeIO ()
contentsRecursive path () = loop path
   where
     loop path = do
         contents path () //> \newPath -> do
             respond newPath
             isDir <- tryIO $ doesDirectoryExist newPath
             let isChild = not $ takeFileName newPath `elem` [".", ".."]
             when (isDir && isChild) $ loop newPath

readFileS
    :: (CheckP p)
    => Int -> FilePath -> () -> Producer (ExceptionP p) B.ByteString SafeIO ()
readFileS chunkSize path () =
    bracket id (openFile path ReadMode) hClose $ \handle -> do
        let loop = do
                eof <- tryIO $ hIsEOF handle
                unless eof $ do
                    bs <- tryIO $ B.hGetSome handle chunkSize
                    respond bs
                    loop
        loop

firstLine :: (Proxy p) => () -> Consumer p B.ByteString IO ()
firstLine () = runIdentityP loop
  where
    loop = do
        bs <- request ()
        let (prefix, suffix) = B8.span  (/= '\n') bs
        lift $ B8.putStr prefix
        if (B.null suffix) then loop else lift $ B8.putStr (B8.pack "\n")


allContent :: (Proxy p) => () -> Consumer p B.ByteString IO ()
allContent () = runIdentityP loop
  where
    loop = do
        bs <- request ()
        lift $ B8.putStr bs


applyScanner :: (Proxy p) => String ->  Consumer p B.ByteString IO ()
applyScanner path = runIdentityP loop
  where
    loop = do
        bs <- request ()
        let sc = tokenToByteString (classify (B8.unpack bs) (initPos path))
        lift $ putStrLn ("Opening file --------------------------------------------------------------- " ++ path)
        lift $ putStrLn ("LO QUE SE LEE ES *********************************************  " ++ (B8.unpack bs))
        lift $ B8.putStrLn sc
        lift $ putStrLn ("Closing file  ---------------------------------------------------------------  " ++ path)


-- ((B8.pack (show ormj)) B8.append (B8.pack " ") B8.append (B8.pack str) B8.append (B8.pack "\t") B8.append (B8.pack pos) B8.append (B8.pack "\n")))
tokenToByteString :: [Token] -> B.ByteString
tokenToByteString ls = foldl (\x (Token ormj str pos) ->
                        B8.append x ( B8.pack ((show ormj) ++ "\t" ++ str ++ "\t" ++ (show pos) ++ "\n")  )) B8.empty ls

handler :: (CheckP p) => FilePath -> Session (ExceptionP p) SafeIO ()
handler path = do
    canRead <- tryIO $ fmap readable $ getPermissions path
    isDir   <- tryIO $ doesDirectoryExist path
    isValidExtension <- tryIO $ evaluate (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
    when (not isDir && canRead && isValidExtension) $
        (readFileS 10240 path >-> try . applyScanner) path


------------------------------------------------------------------------------------------------------------------------



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
