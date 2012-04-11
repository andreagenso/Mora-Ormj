module CreateDomain where

import Data.Char
import Data.List
import UU.Parsing
import GenerateDomain
import System.Directory
import ParseA

-- estructura del dominio
{- Data types -}
data TokDomain = TokDomain SDomain String

data SDomain = Dir
           | LevelProf
           | Sep 
           | Error
           deriving (Eq,Ord)
            
-- LevelProf indica el nivel de profundidad, se representa por un caracter que es '.'
-- Dir es el nombre del directorio 
instance Show TokDomain where 
        show (TokDomain domain str) = show domain ++ " " ++ show str ++ "\n"

-- Name of Result Structure
instance Show SDomain where
        show Dir           = "Directorio : "
        show LevelProf = "Level      : "
        show Sep       = "Separacion : "        
        show Error     = "Error      : " 

-- Scanner The Domain
lp = "."

scanDomain :: String -> IO()
scanDomain domain = loadLexicalStructure domain

loadLexicalStructure  :: String -> IO()
loadLexicalStructure domain = do let tokens = classify domain
                                 putStr(show tokens)
                                                       
classify :: String -> [TokDomain]
classify  ""         = []
classify  tokdomain  | isSep          tokdomain   =  (TokDomain Sep "\n") : (classify (tokdomain \\ ((takeWhile (isSpace) tokdomain))) )
                     | isDomain       tokdomain   =  (TokDomain Dir (takeWhile f tokdomain)) : (classify (tokdomain \\ ((takeWhile f tokdomain))) )
                                         | isLevelProf lp tokdomain   =  (TokDomain LevelProf [(head tokdomain)])        : (classify (tail tokdomain))
                                         | otherwise                  =  (TokDomain Error (" Simbolo desconocido : " ++ tokdomain) )       : (classify "")

isDomain :: String -> Bool
isDomain ""     = False
isDomain tokdomain = (('_' == (head tokdomain)) || (isAlpha (head tokdomain)))

-- ojo con esta funcion, propensa a errores, que pasa si alguna lista es vacia
isLevelProf lp tokdomain = (head lp) == (head tokdomain)

isSep (x:xs) = isSpace x

-- condicion de separacion de modulos de la arquitectura
f x =  (x /= (head lp)) && (not(isSpace x))


-- Integracion Scanner Parser
instance Symbol TokDomain 

instance Eq SDomain  => Eq TokDomain where
  (TokDomain sdom1 cad1) == (TokDomain sdom2 cad2 ) = sdom1 == sdom2 && ( sdom1 == Dir || cad1 == cad2)

instance Ord TokDomain where
  (TokDomain sdom1 cad1) <= (TokDomain sdom2 cad2) = sdom1 < sdom2 || (sdom1 == sdom2 && cad1 <= cad2)

-- parsers integradores
pDir :: Parser TokDomain String
pDir = (\(TokDomain Dir str) -> str) <$> pSym (TokDomain Dir "")

pLevelProf :: String -> Parser TokDomain String
pLevelProf str = (\(TokDomain LevelProf ostr)-> ostr) <$> pSym (TokDomain LevelProf str)

pError :: String -> Parser TokDomain String
pError str = (\(TokDomain Error ostr)-> ostr) <$> pSym (TokDomain Error str)

pSep :: Parser TokDomain String
pSep = (\(TokDomain Sep str) -> str) <$> pSym (TokDomain Sep "\n")

-- ****************************************************************************
-- Parser de domain
-- ****************************************************************************
pRoot = sem_Root_Root <$> pDir <* pSep <*> pDomain

-- pDomain :: Parser TokDomain Domain
pDomain = pFoldr1Sep (sem_Domain_Cons, sem_Domain_Nil) (pLevelProf ".") pDir
           
-- Llamada al compilador
compilador nombre = do entrada <- readFile nombre
                       let sel = classify entrada
                       resultado <- parseIO pRoot sel
                       fst resultado

maincd domain = do 
                       let sel = classify domain
                       resultado <- parseIO pRoot sel
                       fst resultado
                       
getDomain nombre = snd (parseA pRoot (classify nombre))                