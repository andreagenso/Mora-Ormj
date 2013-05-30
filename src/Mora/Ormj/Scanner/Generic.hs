module Mora.Ormj.Scanner.Generic where

import Mora.Ormj.Scanner.Token
import Data.Char
                                
-- Generic Functions

{- verify a token init with a firts Simbol of comment, that may be /*, //, --, %%, #, for Block Comments or Line Comments
   verify a token init with a firts Simbol of SpecialSimbol , that may be <%, , , ., ;, etc.
   verify equal for operators
-}

isToken :: SimCode -> IniSimbol -> Bool
isToken scode       is = is == (take (length is) scode )

-- takeToken token []     = error ("In the process of verify the token exist, but don't exist at the moment of separed tokens!, review the scanner, This case do not be exist (no deberia existir) ")
takeToken :: SimCode -> [Tok] -> Tok
takeToken scode (o:op) | isToken scode o = o
                                           | otherwise       = takeToken scode op

-- update row, especially used for block comment, line comment, specialSymbol, Operator.
-- updateRow :: Row -> Code -> Row
-- updateRow row []     = row
-- updateRow row (x:xs) | x == '\n' = updateRow (row+1) xs
--                      | otherwise = updateRow row xs                     

-- Return True if a Character is a part of rule of identifier                     
isIdentifier' :: Char -> Bool                     
isIdentifier' c = ('_' == c) || (isAlphaNum c) || ('$' == c)