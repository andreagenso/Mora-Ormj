module Main where

import Mora.Ormj.Scanner.Test
import Mora.Ormj.Parser.Test

import Mora.Ormj.Scanner.Position
import Mora.Ormj.Scanner
import UU.Scanner.Position
import System.FilePath ((</>), splitExtension)
import Control.Exception.Base (evaluate)

import Mora.Ormj.Scanner.Token


-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Control.Applicative
-- import System.Environment
-- import System.Directory
-- import System.FilePath
-- import qualified Data.List as L
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.Iteratee as I
-- import Data.Iteratee.Iteratee
-- import qualified Data.Iteratee.Char as EC
-- import qualified Data.Iteratee.IO.Fd as EIO
-- import qualified Data.Iteratee.ListLike as EL

-- import Data.Binary

import Control.Monad (when, unless)
import Control.Proxy
import Control.Proxy.Safe hiding (readFileS)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import System.Directory (readable, getPermissions, doesDirectoryExist)
import System.FilePath ((</>), takeFileName)
import System.Posix (openDirStream, readDirStream, closeDirStream)
import System.IO (openFile, hClose, IOMode(ReadMode), hIsEOF)


-- main :: IO ()
-- main = do f <- getLine
--          let command = test1 f
--           command


test1 "testscanner" = testScanner

test "singlescanner" = testSingleScanner
test "alltypescanner" = testAllTypeScanner


test "singleparser" = testSingleParser
test "alltypeparser" = testAllTypeParser
test "testparser" = testParser

test "tsp0" = tsp0
test "tsp1" = tsp1
test "tsp2" = tsp2
test "tsp2_1" = tsp2_1
test "tsp2_2" = tsp2_2
test "tsp2_3" = tsp2_3
test "tsp2_4" = tsp2_4
test "tsp2_5" = tsp2_5
test "tsp7" = tsp7
test "tsp8" = tsp8
test "tsp9" = tsp9
test "tsp10" = tsp10
test "tsp11" = tsp11
test "tsp17" = tsp17
test "tsp18" = tsp18
test "tsp19" = tsp19
test "tsp21" = tsp21
test "tsp24" = tsp24
test "tsp25" = tsp25
test "tsp26" = tsp26
test "tsp27" = tsp27
test "tsp28" = tsp28
test "tsp29" = tsp29
test "tsp30" = tsp30
test "tsp34" = tsp34
test "tsp35" = tsp35
test "tsp38" = tsp38
test "tsp40" = tsp40
test "tsp47" = tsp47
test "tsp49" = tsp49
test "tsp56" = tsp56
test "tsp58" = tsp58
test "tsp62" = tsp62
test "tsp69" = tsp69
test "tsp70" = tsp70
test "tsp71" = tsp71
test "tsp72" = tsp72
test "tsp76" = tsp76
test "tsp77" = tsp77
test "tsp84" = tsp84
test "tsp85" = tsp85
test "tsp85_1" = tsp85_1
test "tsp85_2" = tsp85_2
test "tsp86_1" = tsp86_1
test "tsp87" = tsp87
test "tsp92" = tsp92
test "tsp96" = tsp96
test "tsp97" = tsp97
test "tsp99" = tsp99
test "tsp102" = tsp102
test "tsp110" = tsp110
test "tsp127" = tsp127
test "tsp132" = tsp132
test "tsp132_1" = tsp132_1
test "tsp133" = tsp133
test "tsp154" = tsp154
test "tsp162" = tsp162
test "tsp166" = tsp166
test "tsp167" = tsp167
test "tsp179" = tsp179
test "tsp193" = tsp193
test "tsp198" = tsp198
test "tsp203" = tsp203
test "tsp204" = tsp204
test "tsp205" = tsp205
test "tspClase" = tspClase
test "tspclase1" = tspclase1
test "tspclase2" = tspclase2
test "tspClass" = tspClass
test "tspEV" = tspEV
test "tspImport" = tspImport
test "tspPI" = tspPI
test "tspPC" = tspPC
test "tspPP" =  tspPP

test "tspj1" = tspj1
test "tspj2" = tspj2
test "tspj3" = tspj3
test "tspj4" = tspj4
test "tspj5" = tspj5
test "tspj" = tspj

-- scanner
test "tss0" = tss0
test "tss1" = tss1
test "tss2" = tss2
test "tss2_1" = tss2_1
test "tss2_2" = tss2_2
test "tss2_3" = tss2_3
test "tss2_4" = tss2_4
test "tss2_5" = tss2_5
test "tss7" = tss7
test "tss8" = tss8
test "tss9" = tss9
test "tss10" = tss10
test "tss11" = tss11
test "tss17" = tss17
test "tss18" = tss18
test "tss19" = tss19
test "tss21" = tss21
test "tss24" = tss24
test "tss25" = tss25
test "tss26" = tss26
test "tss27" = tss27
test "tss28" = tss28
test "tss29" = tss29
test "tss30" = tss30
test "tss34" = tss34
test "tss35" = tss35
test "tss38" = tss38
test "tss40" = tss40
test "tss47" = tss47
test "tss49" = tss49
test "tss56" = tss56
test "tss58" = tss58
test "tss62" = tss62
test "tss69" = tss69
test "tss70" = tss70
test "tss71" = tss71
test "tss72" = tss72
test "tss76" = tss76
test "tss77" = tss77
test "tss84" = tss84
test "tss85" = tss85
test "tss85_1" = tss85_1
test "tss85_2" = tss85_2
test "tss86_1" = tss86_1
test "tss87" = tss87
test "tss92" = tss92
test "tss96" = tss96
test "tss97" = tss97
test "tss99" = tss99
test "tss102" = tss102
test "tss110" = tss110
test "tss127" = tss127
test "tss132" = tss132
test "tss132_1" = tss132_1
test "tss133" = tss133
test "tss154" = tss154
test "tss162" = tss162
test "tss166" = tss166
test "tss167" = tss167
test "tss179" = tss179
test "tss193" = tss193
test "tss198" = tss198
test "tss203" = tss203
test "tss204" = tss204
test "tss205" = tss205
test "tssClase" = tssClase
test "tssclase1" = tssclase1
test "tssclase2" = tssclase2
test "tssClass" = tssClass
test "tssEV" = tssEV
test "tssImport" = tssImport
test "tssPI" = tssPI
test "tssPC" = tssPC
test "tssPP" =  tssPP

test "tssj" = tssj

test _ = print ("Command not found!!")


-- iterate
-- Iterate ------------------------------------------------------------------------------------------------------------
-- getValidContents :: FilePath -> IO [String]
-- getValidContents path =
--     filter (`notElem` [".", "..",".git", ".svn",".metadata",".idea",".project",".gitignore",".settings",".hsproject",".dist-scion",".dist-buildwrapper"])
--     <$> getDirectoryContents path


-- isSearchableDir :: FilePath -> IO Bool
-- isSearchableDir dir = doesDirectoryExist dir

--     -- (&&) <$> doesDirectoryExist dir
--     --     <*> (searchable <$> getPermissions dir)


-- doesFileExistAndFilter :: FilePath -> IO Bool
-- doesFileExistAndFilter dir =
--     (&&) <$> doesFileExist dir
--          <*> return (snd (splitExtension dir) == ".java" || snd (splitExtension dir) == ".mora")


-- getRecursiveContents :: FilePath -> IO [FilePath]
-- getRecursiveContents dir = do
--     cnts <- map (dir </>) <$> getValidContents dir
--     cnts' <- forM cnts $ \path -> do
--         isDirectory <- isSearchableDir path
--         if isDirectory
--             then getRecursiveContents path
--             else if (snd (splitExtension path) == ".java" || snd (splitExtension path) == ".mora")
--                     then return [path]
--                     else return []
--     return . concat $ cnts'

-- printI :: Iteratee [B.ByteString] IO ()
-- printI = do
--     mx <- EL.tryHead
--     case mx of
--          Nothing -> return ()
--          Just l -> do
--              liftIO . B.putStrLn $ l
--              printI




-- firstLineE :: Enumeratee [FilePath] [B.ByteString] IO ()
-- firstLineE = mapChunksM $ \filenames -> do
--     forM filenames $ \filename -> do
--         i <- EIO.enumFile 1024 filename $ joinI $ ((mapChunks B.pack) ><> EC.enumLinesBS) EL.tryHead
--         result <- case i of
--                        Iteratee Nothing -> run I.identity
--                        Iteratee Just l -> do
--                            run l
--         return result

-- enumDir :: FilePath -> Enumerator [FilePath] IO b
-- enumDir dir iter = runIter iter idoneM onCont
--     where
--         onCont k Nothing = do
--             (files, dirs) <- liftIO getFilesDirs
--             if null dirs
--                 then return $ k (Chunk files)
--                 else walk dirs $ k (Chunk files)
--         walk dirs = foldr1 (>>>) $ map enumDir dirs
--         getFilesDirs = do
--             cnts <- map (dir </>) <$> getValidContents dir
--             (,) <$> filterM doesFileExist  cnts
--                 <*> filterM isSearchableDir cnts

-- enumDir :: FilePath -> Enumerator String IO b
-- enumDir dir = list
--   where
--     list (Continue k) = do
--         (files,dirs) <- liftIO getFilesDirs
--         if null dirs
--            then k (Chunks files)
--            else k (Chunks files) >>== walk dirs
--     list step = returnI step
--     walk dirs = foldr1 (<==<) $ map enumDir dirs
--     getFilesDirs = do
--         cnts <- map (dir </>) <$> getValidContents dir
--         (,) <$> filterM doesFileExist cnts
--             <*> filterM isSearchableDir cnts

-- allFirstLines :: FilePath -> IO ()
-- allFirstLines dir = do
--     i' <- enumDir dir $ joinI $ firstLineE printI
--     run i'

-- allFirstLines :: FilePath -> IO ()
-- allFirstLines dir = do
--     filepaths <- getRecursiveContents dir
--     l <- mapM firstLineE $ filepaths
--     mapM_ B.putStrLn l


-- main = do
--     dir:_ <- getArgs
--     allFirstLines dir

testScanner = main -- allFirstLines "/home/andrea/workspaceclipse_haskell/Mora-Ormj"

-- consumer :: Iteratee B.ByteString IO ()
-- consumer = do
--     mw <- EL.head
--     case mw of
--         Nothing -> return ()
--         Just w  -> do
--             liftIO . putStr $ "XXX "
--             liftIO . B.putStrLn . BS.singleton $ w
--             consumer


-- OverloadedStrings allows ByteString literal.
-- listFeeder :: Enumerator B.ByteString IO a
-- listFeeder = enumList 1 [ "12", "34" ]


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
        (readFileS 1024 path >-> try . applyScanner) path

main = runSafeIO $ runProxy $ runEitherK $
      contentsRecursive "/home/andrea/workspaceclipse_haskell/Mora-Ormj/test" />/ handler