-- List of files of directory, like a tree
-- Andrea Moruno tatiana.moruno@gmail.com
-- 2012-04-11
-- Usage ./tList [dir..]
-- Usage ./fList [dir..]

module Main(main) where
 
import System.Directory
import System.Environment
import Data.List
import Control.Monad
import Data.Tree


-- of Data.Tree data Tree a = Node {rootLabel :: a, subForest :: Forest a}

type Path       = String                -- path
type DentName   = String                -- directory entry-name
type DirNode    = (Path, DentName)      -- directory path/dent-name pair
type DirTree    = Tree DentName         -- file-system tree



-- Purely functional tree-to-string formating
showTree :: Tree String -> String
showTree t = unlines (showNode "" "" "" t)

showNode :: String -> String -> String -> Tree String -> [String]
showNode leader tie arm node = do
                nodeRep : showChildren node (leader ++ extension)
        where
                nodeRep = leader ++ arm ++ tie ++ rootLabel node
                extension  = case arm of ""  -> ""; "`" -> "    "; _   -> "|   "
                
showChildren :: Tree String -> String -> [String]
showChildren node leader = 
        let children = subForest node
            arms     = replicate (length children -1 ) "|" ++ ["`"]
        in concat (zipWith (showNode leader "-- ") arms children)                 
                

-- Show path
showTree :: Tree String -> String
showTree t = unlines (showNode "" "" "" t)

showNode :: String -> String -> String -> Tree String -> [String]
showNode leader tie arm node = do
                nodeRep : showChildren node (leader ++ extension)
        where
                nodeRep = leader ++ arm ++ tie ++ rootLabel node
                extension  = case arm of ""  -> ""; "`" -> "    "; _   -> "|   "
                
showChildren :: Tree String -> String -> [String]
showChildren node leader = 
        let children = subForest node
            arms     = replicate (length children -1 ) "|" ++ ["`"]
        in concat (zipWith (showNode leader "-- ") arms children)                 



-- Efectful directory-transversal code that returns a tree
-- representing the directory hierarchy rooted al `path-node`

-- liftM is like : do childForest <- fsTraverseChildren (path ++ "/" ++ node)
--                 return (Node node childForest)

fsTraverse :: Path -> DentName -> IO DirTree
fsTraverse path node =
        Node node `liftM` fsTraverseChildren (path ++ "/" ++ node)
        

fsTraverseChildren :: Path -> IO (Forest DentName)
fsTraverseChildren path =
        mapM (uncurry fsTraverse) =<< fsGetChildren path                

-- helper to get traversable directory entries
fsGetChildren :: Path -> IO [DirNode]
fsGetChildren path = do 
        contents <- getDirectoryContents path `catch` const (return [])
        let visibles = sort . filter (`notElem` [".",".."]) $ contents
        return (map ((,) path) visibles) 


traverseAndPrint :: Path -> IO()
traverseAndPrint  path =
                putStr . showTree =<< fsTraverse root path
        where 
                root = if "/" `isPrefixOf` path then "" else "."
                

-- functions for relation with the scanner
traverseAndShowScanner :: Path -> IO()
traverseAndShowScanner path = 
        scanner  . convertPath =<< fsTraverse root path
        
          
convertPath :: Tree -> String
convertPath t =
                                         

main = do
        args <- getArgs
        mapM_ traverseAndPrint (if null args then ["."] else args)

                      