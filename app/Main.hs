module Main where

import Artist
import UdGraphic
import Test.QuickCheck

-- Joc de prova per la funció separa
-- Ha de retornar [Avança 2, Gira 90, ColorLlapis verd, Para, Gira 45, Avança 5]
jocProvaSeparar :: Bool
jocProvaSeparar = separa ((Avança 2 :#: Gira 90) :#: (Para :#: Gira 45 :#: Avança 5)) == separa(ajunta([Avança 2, Gira 90, Para, Gira 45, Avança 5]))

-- Joc de prova per la funció ajunta
-- Ha de retornar (Avança 2 :#: Gira 90) :#: (ColorLlapis verd :#: Para :#: Gira 45 :#: Avança 5)
jocProvaAjunta :: Bool
jocProvaAjunta = ajunta [Avança 2, Gira 90, Gira 45, Avança 5] == ajunta(separa((Avança 2 :#: Gira 90) :#: (Para :#: Gira 45 :#: Avança 5)))

-- Joc de prova per la funció prop_equivalent
-- Ha de retornar True
jocProvaEquivalent :: Bool
jocProvaEquivalent = prop_equivalent ((Avança 2 :#: Gira 90) :#: (ColorLlapis verd :#: Para :#: Gira 45 :#: Avança 5)) ((Avança 2 :#: Gira 90) :#: (ColorLlapis verd :#: Para :#: Gira 45 :#: Avança 5))

-- Joc de prova per la funció copia
-- Ha de retornar (Avança 2 :#: Avança 2 :#: Avança 2)
jocProvaCopia :: Bool
jocProvaCopia = copia 3 (Avança 2) == (Avança 2 :#: Avança 2 :#: Avança 2)

-- Joc de prova per la funció pentagon
-- Ha de retornar (Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72)
jocProvaPentagon :: Bool
jocProvaPentagon = separa(pentagon 5) == separa(Avança 5.0 :#: Gira 72.0 :#: Avança 5.0 :#: Gira 72.0 :#: Avança 5.0 :#: Gira 72.0 :#: Avança 5.0 :#: Gira 72.0 :#: Avança 5.0 :#: Gira 72.0)

-- Joc de prova per la funció poligon
-- Ha de retornar (Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72)
jocProvaPoligon :: Bool
jocProvaPoligon = separa(poligon 5 5 72) == separa(Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72)

-- Joc de prova per la funció optimitza
-- Ha de retornar (Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72)
jocProvaOptimitza :: Bool
jocProvaOptimitza = optimitza ((Avança 5 :#: Gira 72) :#: (Avança 5 :#: Gira 72) :#: (Avança 5 :#: Gira 72) :#: (Avança 5 :#: Gira 72) :#: (Avança 5 :#: Gira 72)) == (Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72 :#: Avança 5 :#: Gira 72)

-- Executa els jocs de prova
main :: IO ()
main = do
  putStrLn "Joc de prova per la funció separa:"
  quickCheck jocProvaSeparar

  putStrLn "Joc de prova per la funció ajunta:"
  quickCheck jocProvaAjunta

  putStrLn "Joc de prova per la funció prop_equivalent:"
  quickCheck jocProvaEquivalent

  putStrLn "Joc de prova per la funció copia:"
  quickCheck jocProvaCopia

  putStrLn "Joc de prova per la funció pentagon:"
  quickCheck jocProvaPentagon

  putStrLn "Joc de prova per la funció poligon:"
  quickCheck jocProvaPoligon

  putStrLn "Joc de prova per la funció optimitza:"
  quickCheck jocProvaOptimitza
