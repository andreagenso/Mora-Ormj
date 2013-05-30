module Mora.Ormj.Integration.ScannerParser where

import Mora.Ormj.Scanner.Token
import UU.Scanner.Position
import UU.Parsing

-- import Text.ParserCombinators.UU.Core
-- import Text.ParserCombinators.UU.BasicInstances
-- import Text.ParserCombinators.UU.Derived
-- import UU.Parsing.Interface

--  INTEGRACION DEL SCANNER CON EL PARSER

-- file debe llevar el path del archivo actual
-- file = ""

instance Eq Ormj => Eq Token where
  (Token ormj1 cad1 pos1) == (Token ormj2 cad2 pos2) = ormj1 == ormj2 && (  ormj1 == Identifier || 
                                                                ormj1 == BooleanLiteral || ormj1 == CharacterLiteral ||
                                                                ormj1 == StringLiteral  || ormj1 == (DecimalIntegerLiteral )
                                                                || ormj1 == (HexIntegerLiteral )
                                                                || ormj1 == (OctalIntegerLiteral )
                                                                || ormj1 == (DecimalFloatingPointLiteral) 
                                                                || ormj1 == (HexadecimalFloatingPointLiteral) 
--                                                              || ormj1 == NullLiteral  
                                || cad1 == cad2 )

instance Ord Token where
  (Token ormj1 cad1 pos1) <= (Token ormj2 cad2 pos2) = ormj1 < ormj2 || (ormj1 == ormj2 && cad1 <= cad2)
 
instance Symbol Token

-- Los parsers atomicos
obtenerVal (Token _ cad pos) = cad

tSym ::  Ormj -> SimCode -> Parser Token SimCode
tSym ormj str   = obtenerVal <$> pSym (Token ormj str (initPos ""))

pIdentifier                      = tSym Identifier ""
pKeyWord       kw                = tSym KeyWord kw
pSpecialSimbol ss                = tSym SpecialSimbol ss
-- pConstant      co             = pASym Constant  co
pOperator      op                = tSym Operator op
pLineComment                     = tSym LineComment "" 
pBlockComment                    = tSym BlockComment ""
pError                           = tSym Error        ""  
pBooleanLiteral cb               =  (sem_Bool ) <$> tSym BooleanLiteral cb  
pCharacterLiteral                = tSym CharacterLiteral ""
pStringLiteral                   = tSym StringLiteral ""
pTokMayor      op                = tSym TokMayor op



pDecimalIntegerLiteral           =  tSym DecimalIntegerLiteral ""
pHexIntegerLiteral               =  tSym HexIntegerLiteral ""
pOctalIntegerLiteral             =  tSym OctalIntegerLiteral ""
pDecimalFloatingPointLiteral     =  tSym DecimalFloatingPointLiteral "" 
pHexadecimalFloatingPointLiteral =  tSym HexadecimalFloatingPointLiteral ""

{-
pDecimalIntegerLiteral           =  sem_Integer <$> tSym DecimalIntegerLiteral ""
pHexIntegerLiteral               =  sem_Integer <$> tSym HexIntegerLiteral ""
pOctalIntegerLiteral             =  sem_Integer <$> tSym OctalIntegerLiteral ""
pDecimalFloatingPointLiteral     = sem_Floating <$> tSym DecimalFloatingPointLiteral "" 
pHexadecimalFloatingPointLiteral = sem_Floating <$> tSym HexadecimalFloatingPointLiteral ""

-- sem_Integer x = read x :: Integer
-- sem_Floating x = read x :: Double 

-}

pNullLiteral      "null"   = tSym NullLiteral "null"

-- pIntegerLiteral              = tSym (IntegerLiteral pIntLiteral) ""
-- pIntLiteral                  = sem_IntLiteral_DecimalIntegerLiteral
--                            <|> sem_IntLiteral_HexIntegerLiteral
--                            <|> sem_IntLiteral_OctalIntegerLiteral

-- pDecimalIntegerLiteral =  
-- pHexIntegerLiteral     =
-- pOctalIntegerLiteral   =

-- sem_IntLiteral_DecimalIntegerLiteral = DecimalIntegerLiteral
-- sem_IntLiteral_HexIntegerLiteral     = HexIntegerLiteral
-- sem_IntLiteral_OctalIntegerLiteral   = OctalIntegerLiteral
-- pFloatingLiteral             = tSym (FloatingLiteral) ""

-- pOperator'' ">" ">" ">" = tSym Operator "HS@ORMJShift>>>ShiftORMJ@"
-- pOperator'  ">" ">"     = tSym Operator "HS@ORMJShift>>ShiftORMJ@"

sem_Bool x | x == "true" = True
                   | x == "false" = False
                   | otherwise    = error "Boolean Literal error"