module ParseA where 

-- parseA basado en la implementacion de parseIO de UU.PArsing lib
-- Tiene funcionalidad parecida a parseIO de la libreria UU.Parsing, pero en vez
-- de devolver un tipo IO() devuelve un tipo A, probado solo con tipos primitivos.

-- Tatiana Moruno 
-- Comunidad Haskell San Simon
-- version 0.1

import UU.Parsing.Interface

import UU.Parsing.Machine
import UU.Parsing.MachineInterface
import System.IO.Unsafe
import System.IO


parseA :: (Eq s, Show s, Symbol s) => Parser s a -> [s] -> a
parseA = parseAMessage showMessage 
  where showMessage (Msg expecting position action)  
          =  let pos = case position of
                           Nothing -> "at end of file"
                           Just s  -> case action of 
                                Insert _ -> "before " ++ show s
                                Delete t -> "at " ++ show t  
             in "\n?? Error      : " ++ pos ++
                "\n?? Expecting  : " ++ show expecting ++
                "\n?? Repaired by: " ++ show action ++ "\n"                
                

parseAMessage :: ( Symbol s, InputState inp s p) 
               => (Message s p -> String) 
               -> AnaParser inp Pair s p a 
               -> inp 
               -> a
parseAMessage showMessage p inp
 =  par (evalStepsA showMessage (parse p inp))
 
par (Pair v final) = final  `seq` v
   

evalStepsA :: (Message s p -> String) 
            ->  Steps b s p 
            -> b
evalStepsA showMessage = evalStepsA' showMessage (-1)      
       
evalStepsA' :: (Message s p -> String) 
            -> Int
            ->  Steps b s p 
            -> b
            
evalStepsA' showMessage n (steps :: Steps b s p) = eval n steps
  where eval                      :: Int -> Steps a s p -> a
        eval 0 steps               = (evalSteps steps)
        eval n steps = case steps of
          OkVal v        rest -> v (eval n rest)  -- unsafeInterleaveIO
          Ok             rest -> eval n rest
          Cost  _        rest -> eval n rest
          StRepair _ msg rest -> eval (n-1) rest  -- do hPutStr stderr (showMessage msg)
          Best _   rest   _   -> eval n rest
          NoMoreSteps v       -> v
       