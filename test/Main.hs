module Main where

import Mora.Ormj.Scanner.Test

main :: IO ()
main = do f <- getLine
          let command = testscanner f
          command

testscanner "singlescanner" = testSingleScanner
testscanner "alltypescanner" = testAllTypeScanner
testscanner "testscanner" = testScanner
testscanner _ = print ("Command not found!!")

