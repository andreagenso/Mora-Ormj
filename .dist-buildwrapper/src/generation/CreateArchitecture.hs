module CreateArchitecture where

import Data.List 
import Data.Char
import UU.Parsing
import GenerateArchitecture
import ParseA

-- Scanner controlar que la estructura de la arquitectura interna
-- aaa  reconozca solo los simbolos de la forma id/id/id/id

{- Data types -}
data TokExAr = TokExAr ExArDom String

-- arq
data ExArDom = Dir
           | LevelProf
           | Sep
           | Error
           deriving (Eq,Ord)
            
-- LevelProf indica el nivel de profundidad, se representa por un caracter que es '.'
-- Dir es el nombre del directorio 
instance Show TokExAr where 
        show (TokExAr exardom str) = show exardom ++ " " ++ show str ++ "\n"

-- Name of Result Structure
instance Show ExArDom where
        show Dir           = "Directorio : "
        show LevelProf = "Level      : "
        show Sep       = "Separacion : "
        show Error     = "Error      : " 

-- Scanner The Domain
lp = "/"

scanExArDom :: String -> IO()
scanExArDom exardom = loadLexicalStructure exardom

loadLexicalStructure  :: String -> IO()
loadLexicalStructure domain = do let tokens = classify domain
                                 putStr(show tokens)
                                                       
classify :: String -> [TokExAr]
classify ""         = []
classify tokexar  | isSep       tokexar      =  (TokExAr Sep "\n") : (classify (tokexar \\ ((takeWhile (isSpace) tokexar))) )
                                  | isExArDom       tokexar  =  (TokExAr Dir (takeWhile f tokexar)) : (classify (tokexar \\ ((takeWhile f tokexar))) )
                                  | isLevelProf tokexar lp   =  (TokExAr LevelProf [(head tokexar)])        : (classify (tail tokexar))
                                  | otherwise                =  (TokExAr Error (" Simbolo desconocido : " ++ tokexar) )       : (classify "")

-- condicion de separacion de modulos de la arquitectura
f x =  (x /= (head lp)) && (not(isSpace x))

isSep (x:xs) = isSpace x

isExArDom :: String -> Bool
isExArDom ""     = False
isExArDom tokexar = (('_' == (head tokexar)) || (isAlpha (head tokexar)))

isLevelProf lp tokexar = (head lp) == (head tokexar)

-- Integracion Scanner Parser
instance Symbol TokExAr 

instance Eq ExArDom  => Eq TokExAr where
  (TokExAr ead1 cad1) == (TokExAr ead2 cad2 ) = ead1 == ead2 && ( ead1 == Dir || cad1 == cad2)

instance Ord TokExAr where
  (TokExAr ead1 cad1) <= (TokExAr ead2 cad2) = ead1 < ead2 || (ead1 == ead2 && cad1 <= cad2)

-- Parsers integradores :
pDir :: Parser TokExAr String
pDir = (\(TokExAr Dir str) -> str) <$> pSym (TokExAr Dir "")

pLevelProf :: String -> Parser TokExAr String
pLevelProf str = (\(TokExAr LevelProf ostr)-> ostr) <$> pSym (TokExAr LevelProf str)

pError :: String -> Parser TokExAr String
pError str = (\(TokExAr Error ostr)-> ostr) <$> pSym (TokExAr Error str)

pSep :: Parser TokExAr String
pSep = (\(TokExAr Sep str) -> str) <$> pSym (TokExAr Sep "\n")

-- Estructura de datos de la arquitectura externa

-- ****************************************************************************
-- Parser External Architecture 
-- ****************************************************************************

pRoot = sem_Project_Project <$> pDirs <* pSep <*> pArchitecture

pArchitecture = sem_Architecture_Cons <$> pDirs  <*> pArchitecture1
               <|> pSucceed sem_Architecture_Nil

pArchitecture1 = sem_Architecture_Cons <$  pSep  <*> pDirs <*>  pArchitecture1
               <|> pSucceed sem_Architecture_Nil

pDirs = pFoldr1Sep (sem_Dirs_Cons,sem_Dirs_Nil) (pLevelProf "/") pDir

           
-- Llamada al compilador
mainca nombre = do 
               let sel = classify nombre
               resultado <- parseIO pRoot sel
               resultado
--               fst resultado
                       
-- getExtArch nombre = snd (parseA pRoot (classify nombre))