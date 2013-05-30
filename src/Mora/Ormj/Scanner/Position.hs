module Mora.Ormj.Scanner.Position where

import UU.Scanner.Position

-- Tatiana Moruno
-- update row, especially used for block comment, line comment, specialSymbol, Operator.
advBlock :: String -> Pos -> Pos
advBlock ""     pos = pos
advBlock (x:xs) pos |  x == '\n' = advBlock xs (advl 1 pos)
                    | otherwise  = advBlock xs pos


--module ScannerPosition where

-- -- import UU.Scanner.Position
-- import Text.ParserCombinators.UU.BasicInstances

-- -- Tatiana Moruno
-- -- update row, especially used for block comment, line comment, specialSymbol, Operator.
-- -- advBlock :: String -> Pos -> Pos
-- -- advBlock :: String -> LineColPos -> LineColPos
-- advBlock :: String -> Error -> Error
-- advBlock ""     pos = pos
-- advBlock (x:xs) pos |  x == '\n' = advBlock xs pos -- (advl 1 pos)
--                    | otherwise  = advBlock xs pos