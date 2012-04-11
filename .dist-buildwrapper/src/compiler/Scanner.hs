module Scanner(scanner, classify) where

import Char
import List
import ScannerToken
import ScannerShow
import ScannerLexical
import ScannerGeneric
import ScannerPosition
import UU.Scanner.Position
import UU.Parsing

{- Scanner Mora Project, OrmJ Language  -}
-- Init the scan 
scanner :: File -> IO()
scanner file = loadLexicalStructure file

loadLexicalStructure  ::File -> IO()
loadLexicalStructure file = do 
                            reading <- readFile file
                            let tokens = classify reading (initPos file)
                            putStr(show tokens)
                            
{- Init the classify of tokens
f == '\n'   -> review and update the  Row
id Space f  -> review and update the Column
otherwise   -> is a token
-}
classify :: File -> Pos -> Tokens
classify code pos = takeComments (classify1 (verifyUnicode code) pos )

takeComments :: Tokens -> Tokens
takeComments []                          = [] 
takeComments ((Token LineComment _ _ ): ts) = takeComments ts
takeComments ((Token BlockComment _ _): ts) = takeComments ts
takeComments (t:ts)                         = (t:(takeComments ts))

-- verificar is es Unicode, esta funcion se debe corregir porque Unicode != Ascii
verifyUnicode :: File -> File
verifyUnicode []                 = []
verifyUnicode code@(i:input)  | isAscii i = i:(verifyUnicode input)
                              | otherwise = error ("The Ormj language do not support character, review standar unicode")

classify1 :: File -> Pos -> Tokens
classify1 [] pos            = []
classify1 code@(f:file) pos | f == '\n'           = classify file ( advl 1 pos)
                            | isSpace f           = classify file ( advc 1 pos)
                                            | otherwise           = tokClassify (span (not.isSpace) code) pos
                                            
{- Init the classify token by token -}
tokClassify :: (SimCode, Code) -> Pos -> Tokens
tokClassify (scod,code) pos  | isBlockComment   scod                = (Token BlockComment (scod ++ (takeBlockComment code)) pos )                : (classify1 (code \\ (takeBlockComment code)) ( advBlock (takeBlockComment code) pos))
                                                 | isLineComment    scod                = (Token LineComment  (scod ++ (takeLineComment  code)) pos)                 : (classify1 (code \\ (takeLineComment  code)) ( advBlock (takeLineComment  code) pos))
                                                 | isId scod                            = fIniWithId (scod,code) pos
                                                 | isDigitPoint scod                    = fIniWithZero (scod,code) pos
                                                         | isCharacterLiteral scod              = (Token CharacterLiteral (takeCharacterLit scod) pos)                       : (classify1 ((scod \\ (takeCharacterLit scod            )) ++ code) (advc (length (takeCharacterLit scod           )) pos ))                                      
                                                         | isStringLiteral scod code            = (Token StringLiteral    (fst(takeStringLiteral scod code))   pos )         : (classify  ( snd(takeStringLiteral scod code)                    ) (advc (length (fst(takeStringLiteral scod code  ))) pos ))         
                                                         | isMajor       scod majorList         = (Token TokMayor (takeToken scod majorList) pos)                            : (classify1 ((scod \\ (takeToken scod majorList)) ++ code) (advc (length (takeToken scod majorList)) pos ))                                                                                                                                                
                                                         | isOperator       scod operatorList   = (Token Operator (takeToken scod operatorList) pos)                         : (classify1 ((scod \\ (takeToken scod operatorList)) ++ code) (advc (length (takeToken scod operatorList)) pos ))                                                                                          
                                                         | isSpecialSimbol  scod specialSimbol  = (Token SpecialSimbol  (takeToken scod specialSimbol) pos)                                 : (classify1 ((scod \\ (takeToken scod specialSimbol )) ++ code) (advc (length (takeToken scod specialSimbol)) pos ))
                                                 | otherwise                            = (Token Error scod pos)                                                                    : (classify1 code (advc (length scod) pos))

isId [] = False
isId (x:xs) = (isAlpha x) || (x == '_') || (x == '$')

isDigitPoint []     = False
isDigitPoint (x:xs) =  (isDigit  x) || (x == '.')

fIniWithId (scod,code) pos  | isKeyWord        scod keyWord        = (Token KeyWord (takeWhile isAlpha scod) pos)                               : (classify1 ((scod \\(takeWhile isAlpha scod)) ++ code )  (advc (length (takeWhile isAlpha scod )) pos)                )
                                                        | isBooleanLiteral scod constantBool   = (Token BooleanLiteral (takeBooleanLit scod constantBool) pos)              : (classify1 ((scod \\ (takeBooleanLit scod constantBool )) ++ code) (advc (length (takeBooleanLit scod constantBool)) pos ))                                                                                                                                        
                                                        | isNullLiteral    scod ["null"]       = (Token NullLiteral    (takeToken scod ["null"]     ) pos)                  : (classify1 ((scod \\ (takeToken scod ["null"]      )) ++ code) (advc (length (takeToken scod ["null"]     )) pos ))                                                        
                                                        | isIdentifier     scod                = (Token Identifier (takeWhile isIdentifier' scod) pos)                      : (classify1 ((scod \\ (takeWhile isIdentifier' scod)) ++ code) (advc (length (takeWhile isIdentifier' scod )) pos ))
                                                        | otherwise                            = (Token Error scod pos)                                                     : (classify1 code (advc (length scod) pos))

                                                        -- isIdentifier     scod                = (Token Identifier (takeWhile isIdentifier' scod) pos)                      : (classify1 ((scod \\ (takeWhile isIdentifier' scod)) ++ code) (advc (length (takeWhile isIdentifier' scod )) pos ))

fIniWithZero (scod,code) pos  | isOctalIntegerLiteral scod           = (Token (OctalIntegerLiteral)(takeOctInteger scod) pos)                     : (classify1 ((scod \\ (takeOctInteger scod)) ++ code) (advc (length (takeOctInteger scod)) pos ))
                                                      | isHexaFloatingPointLiteral3 scod     = (Token (HexadecimalFloatingPointLiteral)(takeHexaFloatLiteral3 scod) pos)  : (classify1 ((scod \\ (takeHexaFloatLiteral3 scod)) ++ code) (advc (length (takeHexaFloatLiteral3 scod)) pos))
                                                          | isHexaFloatingPointLiteral1 scod     = (Token (HexadecimalFloatingPointLiteral)(takeHexaFloatLiteral1 scod) pos)  : (classify1 ((scod \\ (takeHexaFloatLiteral1 scod)) ++ code) (advc (length (takeHexaFloatLiteral1 scod)) pos))
                                                          | isHexaFloatingPointLiteral2 scod     = (Token (HexadecimalFloatingPointLiteral)(takeHexaFloatLiteral2 scod) pos)  : (classify1 ((scod \\ (takeHexaFloatLiteral2 scod)) ++ code) (advc (length (takeHexaFloatLiteral2 scod)) pos))
                                                          | isHexIntegerLiteral scod             = (Token (HexIntegerLiteral)(takeHexInteger scod) pos)                       : (classify1 ((scod \\ (takeHexInteger scod)) ++ code) (advc (length (takeHexInteger scod)) pos )) 
                                                          | isDecimalFloatingLiteral1 scod       = (Token (DecimalFloatingPointLiteral)(takeFloat1Integer1 scod) pos)         : (classify1 ((scod \\ (takeFloat1Integer1 scod)) ++ code) (advc (length (takeFloat1Integer1 scod)) pos )) 
                                                          | isDecimalFloatingLiteral2 scod       = (Token (DecimalFloatingPointLiteral)(takeFloat1Integer2 scod) pos)         : (classify1 ((scod \\ (takeFloat1Integer2 scod)) ++ code) (advc (length (takeFloat1Integer2 scod)) pos )) 
                                                          | isDecimalFloatingLiteral3 scod       = (Token (DecimalFloatingPointLiteral)(takeFloat1Integer3 scod) pos)         : (classify1 ((scod \\ (takeFloat1Integer3 scod)) ++ code) (advc (length (takeFloat1Integer3 scod)) pos )) 
                                                          | isDecimalFloatingLiteral4 scod       = (Token (DecimalFloatingPointLiteral)(takeFloat1Integer4 scod) pos)         : (classify1 ((scod \\ (takeFloat1Integer4 scod)) ++ code) (advc (length (takeFloat1Integer4 scod)) pos )) 
                                                          | isIntegerLiteral scod                = (Token (DecimalIntegerLiteral)(takeDecimalInteger scod) pos)               : (classify1  ((scod \\ (takeDecimalInteger scod)) ++ code) (advc (length (takeDecimalInteger scod)) pos ))                                                                                                        
                                                          | isSpecialSimbol  scod specialSimbol  = (Token SpecialSimbol  (takeToken scod specialSimbol) pos)                                 : (classify1 ((scod \\ (takeToken scod specialSimbol )) ++ code) (advc (length (takeToken scod specialSimbol)) pos ))
                                                          | otherwise                            = (Token Error scod pos)                                                                    : (classify1 code (advc (length scod) pos))                                         

-- optimizado
-- tieneEspacio  []      = False
-- tieneEspacio (s:scod) | isSpace s = True
--                                        | otherwise = tieneEspacio scod
                                          
tieneEspacio = tieneEspacio' (False)
tieneEspacio' (nil) ls = cata ls where
        cata [] = nil
        cata (l:ls) | isSpace l = True
                    | otherwise = (cata ls)
                                          

-- span' hayEspacios res []       = (res, hayEspacios, [])
-- span' hayEspacios res ('>':xs) = span' hayEspacios ('>':res) xs
-- span' hayEspacios res xxs@(x:xs) | isSpace x = span' True res xs
--                                  | otherwise = (res, hayEspacios, xxs)


{- Auxiliar functions -}
-- verificar :  HexSignificand BinaryExponent FloatTypeSuffixopt
-- Opcion 1 :   HexSignificand    es HexNumeral
isHexaFloatingPointLiteral1 ""             = False
isHexaFloatingPointLiteral1 (c:[])         = False
isHexaFloatingPointLiteral1 (c1:c2:[])     = False
isHexaFloatingPointLiteral1 (c1:c2:c3:cs) | (c1 == '0') && (c2 =='x' || c2 == 'X' ) && (isHexDigit c3) = (isBinaryExponent (dropWhile (isHexDigit) cs))  
                                          | otherwise                                                  = False

-- optimizado
-- isBinaryExponent ""            = False
-- isBinaryExponent (c:[])        = False
-- isBinaryExponent (c1:c2:cs)    = (c1 == 'p' || c1== 'P') && (c2 == '+' || c2 == '-' || (isDigit c2))
isBinaryExponent = isBinaryExponent' (False)
isBinaryExponent' (nil) ls = cata ls where
        cata [] = nil
        cata (l:[]) = nil
        cata (l1:l2:ls) = (l1 == 'p' || l1== 'P') && (l2 == '+' || l2 == '-' || (isDigit l2))

-- no se optimiza
takeHexaFloatLiteral1 ""               = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral1 (c1:[])          = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral1 (c1:c2:[])       = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral1 (c1:c2:c3:[])    = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral1 (c1:c2:c3:cs) = c1:c2:c3:( (takeWhile isHexDigit cs) ++ (takeBinary (dropWhile isHexDigit cs)) )


-- no se optimiza
takeBinary ""            = error ("Invalid  Hexadecimal Floating Literal Number - takeBinary1")
takeBinary (c1:[])       = error ("Invalid  Hexadecimal Floating Literal Number - takeBnary2")
takeBinary (c1:cs)    | (c1 == 'p') || (c1 == 'P') = c1: (signedInteger cs)
                      | otherwise = error ("Invalid  Hexadecimal Floating Literal Number - takeBinary3")

-- no se optimiza
signedInteger ""                                                       = error ("Invalid  Hexadecimal Floating Literal Number")
signedInteger (c1:[])                                                  = error ("Invalid  Hexadecimal Floating Literal Number")
signedInteger (c1:c2:cs) | (isDigit c1)                                = (c1:(takeWhile isDigit (c2:cs))) ++ (floatSuffix (dropWhile isDigit (c2:cs)))
                         | ((c1 == '+') || (c1 == '-'))&& (isDigit c2) = (c1:c2:  (takeWhile isDigit cs)) ++ (floatSuffix (dropWhile isDigit (cs)))
                         | otherwise                                   = error ("Invalid  Hexadecimal Floating Literal Number")


-- no se optimiza
-- verificar :  HexSignificand BinaryExponent FloatTypeSuffixopt
-- Opcion 2 :   HexSignificand    es HexNumeral .
isHexaFloatingPointLiteral2 ""             = False
isHexaFloatingPointLiteral2 (c:[])         = False
isHexaFloatingPointLiteral2 (c1:c2:[])     = False
isHexaFloatingPointLiteral2 (c1:c2:c3:cs) | (c1 == '0') && (c2 =='x' || c2 == 'X' ) && (isHexDigit c3) = (isBinaryExponent   (isPoint (dropWhile (isHexDigit) cs)))  
                                          | otherwise                                                  = False
-- no se optimiza
isPoint "" = ""
isPoint (c1:cs) | c1 == '.' = cs
                | otherwise = ""

-- no se optimiza
takeHexaFloatLiteral2 ""               = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral2 (c1:[])          = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral2 (c1:c2:[])       = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral2 (c1:c2:c3:[])    = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral2 (c1:c2:c3:cs) = c1:c2:c3:( (takeWhile isHexDigit cs) ++ ( take 1 (dropWhile isHexDigit cs)) ++ (takeBinary ( drop 1 (dropWhile isHexDigit cs))) )

-- no se optimiza
-- verificar :  HexSignificand BinaryExponent FloatTypeSuffixopt
-- Opcion 3 :   HexSignificand    es 0x HexDigitsopt . HexDigits  
-- Opcion 3 :   HexSignificand    es 0X HexDigitsopt . HexDigits  
isHexaFloatingPointLiteral3 ""             = False
isHexaFloatingPointLiteral3 (c:[])         = False
isHexaFloatingPointLiteral3 (c1:c2:[])     = False
isHexaFloatingPointLiteral3 (c1:c2:c3:cs) | (c1 == '0') && (c2 =='x' || c2 == 'X' ) && ((isHexDigit c3) || (c3 == '.')) = (isBinaryExponent  ( (dropWhile (isHexDigit) (c3:cs))))  
                                          | otherwise                                                                   = False

-- no se optimiza
isMoreHex "" = ""
isMoreHex (c1:[]) = ""
isMoreHex (c1:c2:cs) | (c1 == '.') && (isHexDigit c2) = (dropWhile isHexDigit cs )
                     | otherwise = ""
                     

-- no se optimiza
takeHexaFloatLiteral3 ""               = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral3 (c1:[])          = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral3 (c1:c2:[])       = error ("Invalid  Hexadecimal Floating Literal Number")
takeHexaFloatLiteral3 (c1:c2:cs) = c1:c2:( (takeWhile isHexDigit cs) ++ 
                                         (take 1 (dropWhile isHexDigit cs))  ++ 
                                         (takeWhile isHexDigit (drop 1 (dropWhile isHexDigit cs))) ++ 
                                         (takeBinary  (dropWhile isHexDigit (drop 1 (dropWhile isHexDigit cs))))
                                         )

-- verify a token is DecimalFloating
-- verify the structure : Digits . Digits(opt) ExponentParts(opt) FloatTypeSuffix(opt)

-- no se optimza
-- verificar si empieza con un digito, seguido de un punto.
isDecimalFloatingLiteral1 (c:[]) = False
isDecimalFloatingLiteral1 cod@(c:code) | (isDigit c) = idflPoint (dropWhile (isDigit) cod)
                                           | otherwise   = False
-- no se optimiza
idflPoint ""     = False
idflPoint (c:[]) = c == '.'
idflPoint (c1:c2:cs) = (c1 == '.') && (  (isDigit c2) || c2 == 'E' || c2 == 'e' || c2 == 'f' || c2== 'F' || c2=='d' || c2=='D')

-- tomar la parte de codigo que corresponde a un floating literal 1 : : Digits . Digits(opt) ExponentParts(opt) FloatTypeSuffix(opt)
-- no se optimiza
takeFloat1Integer1 cod = (takeWhile (isDigit) cod) ++ 
                       (take 1 (dropWhile (isDigit) cod)) ++ 
                       (digitsOpt (drop 1 (dropWhile (isDigit) cod)))


-- no se optimiza
digitsOpt "" = ""
digitsOpt cod@(c:cs) | (isDigit c) = (takeWhile (isDigit) cod) ++ (exponentOpt (dropWhile (isDigit) cod))
                     | otherwise   = "" ++ (exponentOpt (dropWhile (isDigit) cod))

-- no se optimiza
exponentOpt ""         = ""
exponentOpt cod@(c:cs) | (c == 'e') || (c == 'E')                           = c:(signedExponent cs) 
                       | (c =='f') || (c == 'F') || (c =='d') || (c == 'D') = [c]
                       | otherwise                                          = ""

-- Funcion que al menos debe recibir una lista con 2 elementos
-- no se optimiza
signedExponent ""                                                       = error ("Invalid  Floating Literal Number")
signedExponent (c1:[])                                                  = error ("Invalid  Floating Literal Number")
signedExponent (c1:c2:cs) | (isDigit c1)                                = (c1:(takeWhile isDigit (c2:cs))) ++ (floatSuffix (dropWhile isDigit (c2:cs)))
                          | ((c1 == '+') || (c1 == '-'))&& (isDigit c2) = (c1:c2:  (takeWhile isDigit cs)) ++ (floatSuffix (dropWhile isDigit (cs)))
                          | otherwise                                   = error ("Invalid  Floating Literal Number")
-- no se optimiza
floatSuffix ""     = ""
floatSuffix (c:cs) | (c =='f') || (c == 'F') || (c =='d') || (c == 'D') = [c]
                   | otherwise               = ""

-- verify the structure :  . Digits ExponentParts(opt) FloatTypeSuffix(opt)
-- no se optimiza
isDecimalFloatingLiteral2 (c:[])       = False
isDecimalFloatingLiteral2 (c1:c2:code) = (c1 == '.') && (isDigit c2) 

-- no se optimiza
takeFloat1Integer2 (c:cod) = (c : (takeWhile (isDigit) cod) ) ++ (exponentOpt (dropWhile (isDigit) cod))

-- no se optimiza
-- verify the structure :  Digits ExponentParts FloatTypeSuffix(opt)
isDecimalFloatingLiteral3 (c:[])       = False
isDecimalFloatingLiteral3 cod@(c:code) | (isDigit c)  =  idflExponent (dropWhile (isDigit) cod)
                                       | otherwise    = False
-- no se optimiza
idflExponent ""     = False
idflExponent (c:cs) | (c == 'e') || (c == 'E') = isSignNum cs
                        | otherwise                = False
-- no se optimiza
isSignNum ""        = False
isSignNum (c:cs)    = (c == '+') || (c == '-') || (isDigit c)

-- no se optimiza
takeFloat1Integer3 cod = (takeWhile (isDigit) cod) ++ exponentOpt3 (dropWhile (isDigit) cod)

-- no se optimiza
exponentOpt3 ""         = error ("Invalid  Floating Literal Number")
exponentOpt3 cod@(c:cs) | (c == 'e') || (c == 'E')                           = c:(signedExponent cs) 
                        | (c =='f') || (c == 'F') || (c =='d') || (c == 'D') = [c]
                        | otherwise                                          = error ("Invalid  Floating Literal Number")
-- no se optimiza
-- verify the structure :  Digits ExponentParts(opt) FloatTypeSuffix
isDecimalFloatingLiteral4 (c:[])       = False
isDecimalFloatingLiteral4 cod@(c:code) | (isDigit c)  =  idflSuffix (dropWhile (isDigit) cod)
                                       | otherwise    = False
-- no se optimiza
idflSuffix ""         = False
idflSuffix cod@(c:cs) = (idflExponent cod) || (c=='d') || (c=='F') || (c=='f')|| (c=='D')

-- no se optimiza
takeFloat1Integer4 cod = (takeWhile (isDigit) cod) ++ (take 1 ((dropWhile (isDigit) cod)))
                           
-- no se optimiza
-- verify a token is OctalIntegerLiteral
isOctalIntegerLiteral (c:[])     = False
isOctalIntegerLiteral (c1:c2:code) = (c1 == '0') && (isOctDigit c2) 

-- no se optimiza
-- verify a token is HexIntegerLiteral
isHexIntegerLiteral (c:[])     = False
isHexIntegerLiteral (c1:c2:[]) = False
isHexIntegerLiteral (c1:c2:c3:code) = (c1 == '0') && ((c2 == 'x') || (c2 == 'X')) && (isHexDigit c3) 

-- no se optimiza
-- verify a token is IntegerLiteral
isIntegerLiteral (c:[]) = isDigit c
isIntegerLiteral (c1:c2:code) = ((c1 /= '0') && (isDigit c1)) || ((c1 == '0') && (not(isDigit c2)))

-- no se optimiza
-- verify a token is Block Comment
isBlockComment :: SimCode -> Bool
isBlockComment scod = isToken scod "/*"

-- take octal integer
-- no se optimiza
takeOctInteger :: Code -> String
takeOctInteger code  = (take 2 code) ++ (takeWhile isOctDigit (drop 2 code)) ++ takeLong (dropWhile isOctDigit (drop 2 code))

-- take hex integer
-- no se optimiza
takeHexInteger :: Code -> String
takeHexInteger code  = (take 2 code) ++ (takeWhile isHexDigit (drop 2 code)) ++ takeLong (dropWhile isHexDigit (drop 2 code))

-- take decimal integer
-- no se optimiza
takeDecimalInteger :: Code -> String
takeDecimalInteger code  = (takeWhile isDigit code ) ++ takeLong (dropWhile isDigit code)

-- no se optimiza
takeLong ""     = ""
takeLong (c:cs) | (c == 'l') || (c == 'L') = [c]
                | otherwise                = ""

-- no se optimiza
-- take all comment block
takeBlockComment :: Code -> Comment
takeBlockComment []    = ""
takeBlockComment code  | (!!) (code \\ (searchEndComment code)) 1   == '/' = (searchEndComment code) ++ (take 2 (code \\ (searchEndComment code)))
                       | otherwise                                         = (searchEndComment code) ++ ['*'] ++ (takeBlockComment ((code \\ (searchEndComment code))\\ ['*'])) 
                       where  -- search the exit simbol comment
                                    searchEndComment :: Code -> Comment
                                    searchEndComment code = (takeWhile (/= '*') code)

-- no se optimiza
-- verify a token is Line Comment
isLineComment :: SimCode -> Bool 
isLineComment scod      = isToken scod "//"

-- no se optimiza
-- take the line of Comment
takeLineComment :: SimCode -> Comment
takeLineComment = takeWhile ( /='\n')

-- verify a token is a Key Word
-- isKeyWord :: SimCode-> [KeyWord] -> Bool
-- isKeyWord  = (elem)

-- optimizado
isKeyWord :: SimCode-> [KeyWord] -> Bool
--isKeyWord  scod lkw = isKeyWord'  scod "" lkw
isKeyWord  scod lkw =  (elem) (takeWhile isAlpha scod) lkw

-- isKeyWord' :: SimCode-> SimCode -> [KeyWord] -> Bool
-- isKeyWord'  []      scod  lkw   = (elem) scod lkw
-- isKeyWord' (s:scod) sscod lkw   | isAlpha s = isKeyWord' scod (sscod ++ [s]) lkw
--                                              | otherwise = (elem) sscod lkw

-- isKeyWord :: SimCode -> IniSimbol -> Bool
-- isKeyWord scode     is = is == (take (length is) scode ) && ()

-- optimizado
-- verify a token is a SpecialSimbol
isSpecialSimbol :: SimCode -> [SpecialSimbol] -> Bool
isSpecialSimbol scod ss = or (map (isToken scod) ss)

-- optimizado
-- verify a token is a Null Literal
isNullLiteral :: SimCode -> [String] -> Bool
isNullLiteral scod ss = or(map(isToken scod) ss) 

-- optimizado
isBooleanLiteral :: SimCode-> [Constant] -> Bool
isBooleanLiteral scod []                                   = False
isBooleanLiteral scod (c1:cn)  | isBooleanLiteral' scod c1 = True
                                                       | otherwise                 = isBooleanLiteral scod cn                                                   
-- optimizado                                                  
isBooleanLiteral' :: SimCode -> Constant -> Bool
isBooleanLiteral' scod c1  | (length scod) > (length c1)  = ((take (length c1) scod) == c1) &&  not(isAlphaNum( head(drop (length c1) scod))) 
                                                   | (length scod) == (length c1) = (take (length c1) scod) == c1
                                                   | otherwise                      = False                                                    

-- optimizado, solo recibe una lista de 2 elementos para la comparacion
-- take  a BooleanLiteral
takeBooleanLit :: SimCode -> [Constant] -> SimCode
takeBooleanLit scod []                                      =  error "Invalid Boolean Literal"
takeBooleanLit scod (c1:cs) | (take (length c1) scod) == c1 = c1
                                                    | otherwise                     = takeBooleanLit scod cs 

-- optimizado
-- verify a token is a Operator
isOperator :: SimCode -> [Operator] -> Bool
isOperator scod op      =  or (map (isToken scod) op)


isMajor :: SimCode -> [String] -> Bool
isMajor scod op      =  or (map (isToken scod) op)


-- isOperator :: SimCode -> [Operator] -> Bool
-- isOperator scod []     =  False
-- isOperator scod (x:xs) | isToken scod x  = True
--                                 | otherwise       = isOperator scod xs


-- optimizado
-- verify a token is a Identifier
-- The first character of identifier is a letter or _
isIdentifier :: SimCode -> Bool
isIdentifier token = ('_' == (head token)) || (isAlpha (head token)) || ('$' == (head token))

-- isCharacterLiteral
-- isCharacterLliteral se expresa como un caracter o una secuencia de escape encerrados entre comillas simples, los caracteres
-- son valores que estan entre \u0000 \uffff
-- Supplementary characters must be represented either as a surrogate pair within a char sequence, or as an integer,
-- depending on the API they are used with.
-- Las comillas simple se representan por '\x0027' en Hakell
-- Controlar que no sea comilla simple ni una secuencia de escape

-- La diferencia esta en que Java se refiere a una codificacion \u y con haskell se usa \x. Tomar en cuenta 
-- optimizado no es recursivo
isCharacterLiteral :: SimCode -> Bool 
isCharacterLiteral [] = False 
isCharacterLiteral (s0:[]) = False
isCharacterLiteral (s0:s1:[]) = False
isCharacterLiteral (s0:s1:s2:scod) | (s0 == '\x0027') && (s1 == '\\')      = (isScapeSequence (s2:scod)) 
                                                                   | (s0 == '\x0027') && (isInputCharacter s1)  && (s2 == '\x0027') = True                                                                 
                                                                   |otherwise                                                       = False

-- optimizado
isInputCharacter s1 =  (s1 /= '\x0027' && s1 /= '\x005c' && s1 /= '\n' && s1 /= '\r') && ( s1 >= '\x0000' && s1 <= '\xffff')

-- optimizado
isScapeSequence (s3:[]) = False
isScapeSequence cod@(s3:s4:scod)  = (((s3 == 'b')      || (s3 == 't')      ||
                                                                      (s3 == 'n')      || (s3 == 'f')      || 
                                                                      (s3 == 'r')      || (s3 == '\x0022') || 
                                                                      (s3 == '\x0027') || (s3 == '\\')
                                                                      ) && (s4 == '\x0027')) || 
                                                                     (isOctalScape cod) ||
                                                                     (isUTF cod )
-- optimizado                                                                
isUTF []                     = False
isUTF (s1:[])                = False
isUTF (s1:s2:[])             = False
isUTF (s1:s2:s3:[])          = False
isUTF (s1:s2:s3:s4:[])       = False
isUTF (s1:s2:s3:s4:s5:[])    = False
isUTF (s1:s2:s3:s4:s5:s6:ss) = (s1=='u') && (isHexDigit s2) && (isHexDigit s3) && 
                                                           (isHexDigit s4) && (isHexDigit s5) && (s5 /= 'a') &&
                                                           (s5 /= 'd') && (s6=='\x0027')                                                                     

-- optimizado                                                           
isOctalScape []                       = False
isOctalScape (s0:[])                  = False           
isOctalScape (s3:s4:[])               = (isOctDigit s3) && (s4 == '\x0027')                                             
isOctalScape (s3:s4:s5:[])            = ((isOctDigit s3) && (s4 == '\x0027')) ||
                                                                            ((isOctDigit s3) && (isOctDigit s4) && (s5 == '\x0027'))
isOctalScape (s3:s4:s5:s6:scod)       = ((isOctDigit s3) && (isOctDigit s4) && (s5 == '\x0027')) ||
                                                                    ((isOctDigit s3) && (s4 == '\x0027')) ||
                                                                    ((isZeroThree s3) && (isOctDigit s4) && (isOctDigit s5) && (s6 == '\x0027'))

-- optimizado
isZeroThree car = (car == '0') || (car == '1') ||(car == '2')||(car == '3')
                         
-- optimizado
isSingleQuote  []               = False
isSingleQuote  (s1:[])          = False
isSingleQuote  (s1:s2:[])       = False
isSingleQuote  (s1:s2:s3:[])    = False
isSingleQuote  (s1:s2:s3:s4:ss) = (s1 == '\x0027') && (s2 == '\\') && (s3 == '\x0027') && (s4 == '\x0027')

-- optimizado
takeCharacterLit cod@(s1:s2:s3:scod) | (s1 == '\x0027') && (isInputCharacter s2)  && (s3 == '\x0027')      = (s1:s2:s3:[])  
                                                                 | isSingleQuote cod                                                   =  (take 4 cod) 
                                                                 | (s1 == '\x0027') && (s2 == '\x005c') && (isScapeSequence (s3:scod)) = (s1:s2:[]) ++ (takeWhile (/= '\x0027') (s3:scod)) ++ [(head ((s3:scod) \\ (takeWhile (/= '\x0027') (s3:scod))))]
                                                                 |otherwise                                                            = error "caracter desconocido"
                                                                 

-- Funciones para probar si es un StringLiteral
isStringLiteral []      _       = False
isStringLiteral cod@(s1:scod)  code | (s1 == '"') && (isEndQuote scod)                         = checkString (takeEndQuote scod)
                                                                        | (s1 == '"') && not(isEndQuote scod) && (isEndQuote code) = checkString (scod ++ (takeEndQuote code))
                                                                        | otherwise                                                = False -- la cadena no se cierra

-- Funcion auxiliar que verifica que la existencia del cierre de una cadena considerando
-- la doble comilla
                   
-- optimizado
isEndQuote [] = False
isEndQuote (s1:[]) = s1 == '"'
isEndQuote (s1:s2:scod) | s1 == '\\' && s2 =='"' = isEndQuote scod 
                                                | s1 == '"'              = True
                                                | otherwise              = isEndQuote (s2:scod)

-- No se devuelve la comilla doble de cierre
takeEndQuote []           = error "Invalid StringLiteral"
takeEndQuote (s1:[])      | s1 == '"' = ([])
                                              | otherwise = error "Invalid StringLiteral"
takeEndQuote (s1:s2:scod) | s1 == '\\' && s2 =='"' = (s1:s2:(takeEndQuote scod)) 
                                                  | s1 == '"'              = ([])
                                                  | otherwise              = (s1:(takeEndQuote (s2:scod)))                                         


-- caso base, cadena vacia ""
checkString []        = True
checkString cod | isInputCharacter' cod = checkString (tail cod)
                                | isScapeSequence' cod  = checkString (tail (tail cod))
                                | isOctalScape' cod     = checkString (takeOctal cod)
                                | isUTF'         cod    = checkString ( cod \\ (take 6 cod))
                                | isSingleQuote' cod    = checkString (tail (tail cod))
                                | otherwise             = False

-- Optimizado
-- \x005c es backSlash
isInputCharacter' []        = False 
isInputCharacter' (s1:scod) =  s1 /= '\x005c' && s1 /= '\n' && s1 /= '\r' && s1 /= '\"'  && ( s1 >= '\x0000' && s1 <= '\xffff')

-- Optimizado
isScapeSequence'  []      = False
isScapeSequence' (s1:[]) = False
isScapeSequence' cod@(s1:s2:scod)  = (s1 == '\\') && (  (s2 == 'b') || (s2 == 't') ||
                                                                                                        (s2 == 'n') || (s2 == 'f') || 
                                                                                                        (s2 == 'r') || (s2 == '\"') || 
                                                                                                        (s2 == '\x0027') || (s2 == '\\')
                                                                                                  )
                                                                                                  
takeOctal :: String -> String
takeOctal []         = error ("invalid String for Octal Value ")
takeOctal (s1:[])    = error ("invalid String for Octal Value ")
takeOctal (s1:s2:[]) |(s1 == '\\') && (isOctDigit s2) = []
                                         | otherwise                      = error ("invalid String for Octal Value ")
takeOctal (s1:s2:s3:[]) | (s1 == '\\') && (isOctDigit s2) && not(isOctDigit s3) = (s3:[])
                                                | (s1 == '\\') && (isOctDigit s2) && (isOctDigit s3)    = []
                                                | otherwise                                             = error ("invalid String for Octal Value ")
takeOctal (s1:s2:s3:s4:scod)    | (s1 == '\\') && (isOctDigit s2) && not(isOctDigit s3)                    = (s3:s4:scod)
                                                                | (s1 == '\\') && (isOctDigit s2) && (isOctDigit s3) && not(isOctDigit s4) = (s4:scod)
                                                            | (s1 == '\\') && (isZeroThree s2) && (isOctDigit s3) && (isOctDigit s4)   = scod
                                                            | otherwise                                                                = []          
                                                
isUTF' :: String -> Bool
isUTF' []                     = False
isUTF' (s1:[])                = False
isUTF' (s1:s2:[])             = False
isUTF' (s1:s2:s3:[])          = False
isUTF' (s1:s2:s3:s4:[])       = False
isUTF' (s1:s2:s3:s4:s5:[])    = False
isUTF' (s0:s1:s2:s3:s4:s5:scod) = (s0 == '\\') && (s1=='u') && (isHexDigit s2) && (isHexDigit s3) && 
                                                           (isHexDigit s4) && (isHexDigit s5) && (s5 /= 'a') &&
                                                           (s5 /= 'd')                                                               
                                                                
isOctalScape' :: String -> Bool
isOctalScape' []                    = False
isOctalScape'  (s1:[])               = False
isOctalScape' (s1:s2:[])            = (s1 == '\\') && (isOctDigit s2)                                           
isOctalScape' (s1:s2:s3:[])         | (s1 == '\\') && (isOctDigit s2) && not(isOctDigit s3) = True
                                                                        | (s1 == '\\') && (isOctDigit s2) && (isOctDigit s3)    = True
                                                                        | otherwise                                             = False
                                                                            
isOctalScape' (s1:s2:s3:s4:scod)        | (s1 == '\\') && (isOctDigit s2) && not(isOctDigit s3)                    = True
                                                                        | (s1 == '\\') && (isOctDigit s2) && (isOctDigit s3) && not(isOctDigit s4) = True
                                                                | (s1 == '\\') && (isZeroThree s2) && (isOctDigit s3) && (isOctDigit s4)   = True
                                                                | otherwise                                                                = False          
                         
isSingleQuote' :: String -> Bool 
isSingleQuote'  []               = False
isSingleQuote'  (s1:[])          = False
isSingleQuote'  (s1:s2:ss)       = (s1 == '\\') && (s2 == '\x0027')

-- No puede darse el caso de que span devuelva "" en el elemento segundo de la tupla
-- takeStringLiteral :: String -> String -> (String, String)
-- takeStringLiteral sc@(s:scod) code | (s == '"') && (isEndQuote scod)                            = ((s:(fst(span (/= '\"') scod))) ++ "\"", (tail (snd(span (/= '\"') scod))) ++ code)
--                                                                 | (s == '"') && not(isEndQuote scod) && (isEndQuote code) = ((sc ++ (fst(span (/= '\"') code)) ++ "\""), (tail (snd(span (/= '\"') code))) )
--                                                                 | otherwise                                                  = error "Invalid String Literal"

takeStringLiteral :: String -> String -> (String, String)
takeStringLiteral sc@(s:scod) code | (s == '"') && (isEndQuote scod)                         = ( ((s:(takeEndQuote scod)) ++ "\""   ),( (tail(scod \\ (takeEndQuote scod))) ++ code))
                                                                   | (s == '"') && not(isEndQuote scod) && (isEndQuote code) = ( (sc ++ (takeEndQuote code) ++ "\""), ( (tail(code \\ (takeEndQuote code)))        ))
                                                                   | otherwise                                                  = error "Invalid String Literal"


-- isStringLiteral []      _       = False
-- isStringLiteral cod@(s1:scod)  code | (s1 == '"') && (isEndQuote scod)                         = checkString (takeEndQuote scod)
--                                                                      | (s1 == '"') && not(isEndQuote scod) && (isEndQuote code) = checkString (scod ++ (takeEndQuote code))
--                                                                      | otherwise                                                = False -- la cadena no se cierra