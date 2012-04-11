module Main where

import CreateDomain (maincd, getDomain)
import CreateArchitecture (mainca)
-- modulo Main para la creacion de la arquitectura externa, dominio, arquitectura interna.

-- main funcion principal
main :: IO()
main = do
         welcome
         mora

-- mora : funcion donde se inicia la interaccion de instrucciones y se muestra el prompt
mora :: IO()
mora = do 
         putStr "$>"    
         inst <- getLine
         putStr "$>"
         console inst

-- console : funcion que diferencia una instruccion de mora interactive con instrucciones de ormj
console :: String -> IO()       
console inst = if (head inst) == ':' 
      then runCommandsMora (tail inst) 
      else (if (inst /= []) then (interacOrmj inst) else (nothing inst))

-- interacOrmj : toma todas las instrucciones de ormj y las ejecuta
interacOrmj :: String -> IO()
interacOrmj inst | inst == "cp" = cp
                 | otherwise =  mora

-- nothing funcion que controla que el usuario no escribe nada
nothing :: String -> IO()
nothing inst1 = do
        inst2 <- getLine 
        console (inst1++inst2)

-- runCommandsMora : se ejecutan los comandos de mora interactive :? :a y otros
runCommandsMora :: String -> IO()
runCommandsMora inst | inst == "q" = leave
                             | inst == "h" || inst == "?" = help
                             | inst == "a" = about
                             | otherwise = badCommand

-- leave : salir de mora interactive
leave :: IO()
leave = do 
        putStr "Leaving MORA-ORMJ ...bye \n"

-- help : Ayuda de mora Interactive
help::IO()
help = do 
        putStr "\nCommands of Mora \n"
        putStr ":? or :h print help\n"
        putStr ":q leave the program\n"
        putStr ":a about the program an the authors\n"
        mora

-- about : Acerca de los autores
about :: IO()
about = do
        putStr "\n Authors of Mora-OrmJ :\n"
        putStr "Tatiana Andrea Moruno Rodriguez tatiana.moruno@gmail.com \n"
        mora

-- badCommand : comando incorrecto intente de nuevo pinche!
badCommand :: IO()
badCommand = do
        putStr "Bad command try again \n"
        mora

-- create Project, recibe el nombre del proyecto        
-- paso 1 : se ingresa el nombre del proyecto
-- paso 2 : se crea el proyecto y la qrquitectura externa por defecto.
-- paso 3 : se ingresa el nombre del dominio
-- paso 4 : se ingresa la arquitectura interna por defecto

cp   = do 
      putStrLn "Paso 1 : Ingrese el nombre del proyecto ej: jpetstore : "
      name <- getLine
      let externArch = name ++ " db/postgres  src   war/WEB-INF/jsp/spring   war/WEB-INF/lib  war/images"
      mainca externArch
      putStrLn "Se ha creado el proyecto y la arquitectura externa"
      putStrLn "Paso 2 : Ingrese el dominio del proyecto ej: org.sprignframework.java  : "
      dominio <- getLine
      let completeDomain = name ++ "/src "++ dominio
      maincd completeDomain
      let internArch = (getDomain completeDomain) ++ "   dao/ibatis/maps domain/logic  service/client web/spring"
      print ("Creando La arquitectura interna :" ++ internArch)
      mainca internArch
      putStrLn "OK arquitectura MVC completamente creada"
      mora

-- ****************************************************************
--  Welcome
-- ****************************************************************
welcome = do 
         putStrLn "    __   __ ____ ____  __       ____ ____  __   __ _____ \\|/                                     "
         putStrLn "   /*/| / //*__//*___//*_|     / __// ___// /| / //__/   ~ ~   MORA-ORMJ Interactive, version 0.1 "
         putStrLn "  /*/ |/ //*/ //*/__//*/_| __ / / // /__// / |/ /   /    @-@   http://www.mora_ormj.org           "
         putStrLn " /*/    //*/_//*/ | /*/  |/_// /_// / | / /    ///_/     mmm   Type :? for help                   "
         putStrLn "/_/    //___//_/  |/_/   |  /___//_/  |/_/    //__/                                               "
         putStrLn " "