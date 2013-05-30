module Chain where

import UU.Parsing
import Data.Char

main = do let tokens = "s*s*C"
          resultado <- parseIO pSE tokens
          putStrLn . show $ resultado

instance Symbol Char

-- Sintaxis concreta

data Class = Class
          deriving Show
          
{-  pRE    :: Parser Char RE
pRE    = pChainl pOp pClass          

pSE    :: Parser Char RE
pSE    = SE <$ pSym 's'

pOp    :: Parser Char (RE -> Class -> RE)
pOp    = (:*:) <$ pSym '*'

pClass :: Parser Char Class
pClass = Class <$ pSym 'C'

data RE    = SE
          | RE :*: Class
          deriving Show -}

pRE    = pChainl pOp pSE -- pClass

pSE    :: Parser Char RE
pSE    = SE <$ pSym 's'

-- pOp    :: Parser Char (RE -> Class -> RE)
pOp    = (:*:) <$ pSym '*'

pClass :: Parser Char Class
pClass = Class <$ pSym 'C'

data RE    = SE
          | RE :*: RE
          deriving Show