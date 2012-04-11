module ScannerLexical where

import ScannerToken 

-- Reserved Simbols of language	
keyWord :: [KeyWord]

keyWord = ["delete","update","insertion","view",
           "facadeGen","controllerGen","persistGen","controller",
           "facade","persist","key","project",
           "abstract","continue","for","new","switch",
           "assert","default","if","package","synchronized",
           "boolean","do","goto","private","this",
           "break","double","implements","protected","throw",
           "byte","else","import","public","throws",
           "case","enum","instanceof","return","transient",
           "catch","extends","int","short","try",
           "char","final","interface","static","void",
           "class","finally","long","strictfp","volatile",
           "const","float","native","super","while", "int","long", "short"
           ]

specialSimbol :: [SpecialSimbol]
specialSimbol = [",",";","{","}","<%","%>",".","(",")","=","[","]","@"]

constantBool ::[Constant]
constantBool = ["true","false"]

-- Operatos
---- operatorSingle :: [Operator]
---- operatorSingle = [">","<","!","~","?",":","+","-","*","/","&","|","^","%"] -- se ha quitado =
	
---- operatorDoble :: [Operator]
-- operatorDoble = ["==","<=",">=","!=","+=","-=","*=","/=","&=","|=","^=","%=","&&","||","++","--","<<", "HS@ORMJShift>>ShiftORMJ@"]
---- operatorDoble = ["==","<=",">=","!=","+=","-=","*=","/=","&=","|=","^=","%=","&&","||","++","--","<<",">>"]

---- operatorTri :: [Operator]
---- operatorTri = ["<<=",">>=",">>>=", "HS@ORMJShift>>>ShiftORMJ@"]
-- operatorTri = ["<<=",">>=",">>>=",">>>"]

---- operatorQuard :: [Operator]
---- operatorQuard = [">>>="]
operatorList :: [Operator]
-- operatorList = [">>>=","<<=",">>=",">>>=",">>>","==","<=",">=","!=","+=","-=","*=","/=","&=","|=","^=","%=","&&","||","++","--","<<",">>",">","<","!","~","?",":","+","-","*","/","&","|","^","%"]
operatorList = [">>>=","<<=",">>=",">>>=","==","<=",">=","!=","+=","-=","*=","/=","&=","|=","^=","%=","&&","||","++","--","<<","<","!","~","?",":","+","-","*","/","&","|","^","%"]	

majorList :: [String]
majorList = [">>>",">>",">"]


digits :: [String]
digits = ["0","1","2","3","4","5","6","7","8","9"]