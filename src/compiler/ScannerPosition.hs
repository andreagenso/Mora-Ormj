module ScannerPosition where

import UU.Scanner.Position

-- Tatiana Moruno
-- update row, especially used for block comment, line comment, specialSymbol, Operator.
advBlock :: String -> Pos -> Pos
advBlock ""     pos = pos  
advBlock (x:xs) pos |  x == '\n' = advBlock xs (advl 1 pos)
                    | otherwise  = advBlock xs pos                     